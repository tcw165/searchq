;;; search.el --- Framework of queued search tasks using GREP, ACK, AG and more.
;;
;; Copyright (C) 2014-2015
;;
;; Author: boyw165
;; Version: 20150209.1800
;; Package-Requires: ((emacs "24.3") (hl-anything "0.0.9"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  I not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; A search framework which let you search string or regular expression
;; in the background and queue the search tasks. It uses `find`, `grep`,
;; `ack` or `ag` command as its searching backends. In addition, the
;; search result can be a jotting. So it keeps the search result for
;; you and provides editing function.
;;
;; TODO
;; ----
;; * Support ACK.
;; * Support AG.
;; * Improve `search-result-mode'.
;; * Cancel individual search task.
;; * `search-toggle-search-result' shouldn't always kill search buffer.
;; * Add menu items and toolbar buttons.
;; * Open with search-result will cause hl-highlight-mode work incorrectly.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2015-02-15
;; * Initial release.
;; * Support queued search task.
;; * Support both of asynchronous process and synchronous script task.
;; * Support killing asynchronous search task.
;; * Support imenu for search result.
;;
;;; Code:

;; GNU library.
(require 'ido)

;; 3rd party libary.
(require 'hl-anything)

(defgroup search nil
  "Search")

(defconst search-default-backends
  '(("FIND and GREP" ("find" "grep") "find `pwd`|xargs grep -nH -e ${cursor}" search-grep-backend)
    ("ACK only"      ("ack")         "ack --nocolor ${cursor} `pwd`"          search-ack-backend)
    ("AG only"       ("ag")          ""                                       search-ag-backend))
  "Default search backends. The format:
The 1st element is description string.
The 2nd element is a exec path list to be tested.
The 3rd element is a command template string.
The 4th element is the backend which is the doer of everything.")

(defconst search-buffer-name "*Search Result*"
  "Search buffer name.")

(defconst search-temp-buffer-name "*Search Temp*"
  "Temporary search buffer name.")

(defcustom search-backends (nth 0 search-default-backends)
  "Search backends. See `search-default-backends'."
  :type `(choice ,@(mapcar (lambda (c)
                             `(const :tag ,(nth 0 c)
                                     ,c))
                           search-default-backends)
                 (list :tag "User Defined"
                       (string :tag "Description")
                       (repeat :tag "Necessary Exec List" (string :tag "Name"))
                       (string :tag "Command Sample")
                       (function :tag "Backend Function")))
  :group 'search)

(defcustom search-ignored-paths-for-find-command '("*.git*" "*.svn*")
  "Ignored paths for FIND command."
  :type '(repeat string)
  :group 'search)

(defcustom search-saved-file (expand-file-name "~/.emacs.d/.search")
  "File for cached search result."
  :type 'string
  :set (lambda (symb val)
         (when (file-writable-p val)
           (set symb val)
           (add-to-list 'auto-mode-alist
                        `(,(format "\\%s\\'" (file-name-nondirectory val))
                          . search-result-mode))))
  :group 'search)

(defcustom search-temp-file (expand-file-name "/var/tmp/.search-tmp")
  "Temporary file for search. e.g. as an input file with context of files 
list"
  :type 'string
  :group 'search)

(defcustom search-tasks-max 5
  "Maximum length of the task queue."
  :type 'integer
  :group 'search)

(defcustom search-timer-delay 0.3
  "Delay seconds for every search task."
  :type 'integer
  :group 'search)

(defcustom search-delimiter '(">>>>>>>>>> " . "<<<<<<<<<<")
  "Delimiter of every search task. Default is markdown style."
  :type '(cons (match :tag "open delimiter")
               (match :tag "close delimiter"))
  :group 'search)

(defcustom search-start-action-function 'search-switch-to-search-buffer
  "Action function whenever new search starts."
  :type 'function
  :group 'search)

(defcustom search-prompt-function 'search-default-prompt-function
  "Prompt function."
  :type 'function
  :group 'search)

(defvar search-tasks nil
  "Search tasks queue. See `search-create-task' for struct format.")

(defvar search-tasks-count 0
  "Search tasks count.")

(defvar search-proc nil
  "Search task process.")

(defvar search-timer nil
  "A delay timer for evaluating the queued tasks.")

(defvar search-prompt-timer nil
  "A timer for showing prompt animation.")

(defun search-exec? ()
  "[internal use]
Test whether the necessary exe(s) are present."
  (unless (and (executable-find "sh")
               (executable-find "xargs")
               (null (memq nil
                           (mapcar 'executable-find
                                   (nth 1 search-backends)))))
    (error "%s or xargs is not supported on your system!"
           (nth 1 search-backends)))
  t)

(defun search-running? ()
  "[internal use]
Test whether the search is under processing."
  (> search-tasks-count 0))

(defmacro search-with-search-buffer (&rest body)
  "[internal use]
Evaluate BODY in the search buffer."
  (declare (indent 0) (debug t))
  `(with-current-buffer (get-buffer-create search-buffer-name)
     (set-auto-mode t)
     (setq buffer-file-name (expand-file-name search-saved-file))
     ,@body))

(defun search-prepare-search-buffer ()
  "Prepare search buffer."
  (unless (get-buffer search-buffer-name)
    (search-with-search-buffer
      (and (file-exists-p search-saved-file)
           (insert-file-contents-literally
            search-saved-file
            nil nil nil t)))))

(defun search-create-task (func async)
  "[internal use]
Create a search task with FUNC function and ASYNC boolean."
  (list :func func
        :async async))

(defun search-task-p (task)
  "[internal use]
Test if the TASK is a valid search task."
  (plist-get task :func))

(defun search-append-task (task)
  "[internal use]
Append TASK to `search-tasks' and evaluate it later. See `search-create-task'."
  (when (search-task-p task)
    ;; Append task and update destruct task.
    (setq search-tasks (delq search-destructor-task search-tasks)
          search-tasks (append search-tasks
                               (list task search-destructor-task)))))

(defun search-list-tasks ()
  )

(defun search-switch-to-search-buffer ()
  (switch-to-buffer search-buffer-name))

(defun search-doer ()
  "[internal use]
Doer decide when and what to process next."
  (let* ((task (car search-tasks))
         (func (plist-get task :func)))
    (pop search-tasks)
    ;; Execute current task.
    (condition-case err
        (and (functionp func)
             (funcall func))
      (error (message "search-doer error: %s"
                      (error-message-string err))))
    ;; Find next task.
    (when search-tasks
      (cond
       ((null (plist-get task :async))
        (search-setup-doer))))))

(defun search-setup-doer ()
  "[internal use]
Run `search-doer' after a tiny delay."
  (and (timerp search-timer)
       (setq search-timer (cancel-timer search-timer)))
  (setq search-timer (run-with-idle-timer
                      search-timer-delay nil
                      'search-doer)))

(defun search-start-dequeue ()
  "[internal use]
Start to evaluate search task in the queue."
  ;; Setup timer
  (search-setup-doer)
  ;; Start prmopt.
  (search-start-prompt))

(defvar search-prompt-animation '("-" "\\" "|" "/")
  "[internal use]
Prompt animation.")

(defun search-message (message &rest args)
  "[internal use]
Display MESSAGE in the minibuffer when minibuffer is inactive."
  (when (not (minibufferp (current-buffer)))
    (if args
        (apply 'message message args)
      (message "%s" message))))

(defun search-default-prompt-function ()
  "[internal use]
Default prompt function."
  (let ((char (car search-prompt-animation)))
    (search-message "Search ...%s" char)
    (setq search-prompt-animation (cdr search-prompt-animation)
          search-prompt-animation (append
                                   search-prompt-animation
                                   (list char)))))

(defun search-start-prompt ()
  "[internal use]
Start prmopt animation."
  (when (timerp search-prompt-timer)
    (setq search-prompt-timer (cancel-timer search-prompt-timer)))
  (setq search-prompt-timer
        (run-with-timer
         0 0.1
         (lambda ()
           (and (functionp search-prompt-function)
                (funcall search-prompt-function))))))

(defun search-stop-prompt ()
  "[internal use]
Stop prompt animation."
  (when (timerp search-prompt-timer)
    (setq search-prompt-timer (cancel-timer search-prompt-timer)))
  (search-message "Search ...done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Task API for Backends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun search:chain (&rest tasks)
  "[backend api]
Chain the TASKS."
  (mapc 'search-append-task tasks)
  (search-start-dequeue)
  nil)

(defmacro search:lambda (&rest body)
  "[backend api]
Create a search task wrapping FUNC which is a lambda function."
  ;; Sample ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; (search:lambda
  ;;   (message "123"))
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  (declare (doc-string 2) (indent defun) (debug t))
  `(search-create-task
    ;; Function.
    (lambda ()
      (condition-case err
          (progn ,@body)
        (error (message "search:lambda | error: %s"
                        (error-message-string err)))))
    ;; Synchronous.
    nil))

(defmacro search:lambda-to-search-buffer (&rest body)
  "[backend api]
Create a search task wrapping FUNC under `search-buffer'."
  (declare (doc-string 2) (indent defun) (debug t))
  `(search:lambda
     (search-with-search-buffer
       (condition-case err
           (save-excursion
             ,@body)
         (error (message "search:lambda-to-search-buffer | error: %s"
                         (error-message-string err)))))))

(defun search:process-shell (command bufname &optional callback)
  "[backend api]
Create a search task wrapping `start-process-shell-command' with COMMAND.
The output will be dumpped to a BUFNAME buffer which will be deleted when done.
The CALLBACK is evaluated under process's buffer.
See `search:process-shell-to-file' or `search:process-shell-to-search-buffer'
for example."
  (search-create-task
   ;; Function.
   (eval
    `(lambda (&rest args)
       (let* ((buf ,(if (string= bufname search-buffer-name)
                        (search-with-search-buffer
                          (current-buffer))
                      (get-buffer-create (or bufname
                                             search-temp-buffer-name)))))
         (and (process-live-p search-proc)
              (setq search-proc (delete-process search-proc)))
         (setq search-proc (start-process-shell-command
                            "*search-proc*" buf ,command))
         (set-process-sentinel
          search-proc
          (lambda (proc event)
            (with-current-buffer (process-buffer proc)
              (condition-case err
                  ,(and (functionp callback)
                        `(funcall ,callback))
                (error (message "search:process-shell | error: %s"
                                (error-message-string err))))
              ;; Kill buffer if it is a temporary buffer.
              (and (string= search-temp-buffer-name
                            (buffer-name))
                   (kill-buffer)))
            (search-setup-doer))))))
   ;; Asynchronous.
   t))

(defun search:process-shell-to-file (command filename)
  "[backend api]
Create a search task wrapping `start-process-shell-command' with COMMAND.
The output will be written to FILENAME file."
  (search:process-shell
    command (file-name-nondirectory filename)
    (eval `(lambda ()
             ;; Prepend the content if file is alreay existed.
             (when (file-exists-p ,filename)
               (goto-char (point-min))
               (insert-file-contents-literally))
             (setq buffer-file-name ,filename)
             (save-buffer)
             (kill-buffer)))))

(defun search:process-shell-to-search-buffer (command)
  "[backend api]
Create a search task wrapping `start-process-shell-command' with COMMAND.
The output will be dumpped directly to the `search-buffer'."
  (search:process-shell
    command search-buffer-name
    (lambda ()
      (setq buffer-file-name (expand-file-name search-saved-file)))))

(defvar search-clean-temp-file-task
  (search:lambda
    (and (file-exists-p search-temp-file)
         (delete-file search-temp-file)))
  "[backend api]
Search task to clean temporary file.")

(defvar search-print-closed-delimiter
  (search:lambda-to-search-buffer
    (goto-char (point-max))
    (unless (looking-back "[\r\n]")
      (insert "\n"))
    (insert (cdr search-delimiter) "\n\n")
    (save-buffer))
  "[backend api]
Search task to print closed delimiter.")

;; !important! The last search task.
(defvar search-destructor-task
  (search:lambda
    ;; Stop prompt.
    (search-stop-prompt)
    ;; Reset counter.
    (setq search-tasks-count 0))
  "[internal use]
Destructor-liked search task which is always the last one in the queue.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun search-dummy-backend (&rest args)
  "[internal use]
Just a dummy backend. See `search-thing'.")

(defun search-gen-find-filter (include exclude)
  "[internal use]
Take INCLUDE and EXCLUDE arguments and generate FIND command string. The format
depends on the FIND's option, --path.
e.g. *.txt is for GREP command."
  ;; Sample ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; (search-gen-find-filter nil nil)
  ;; (search-gen-find-filter '("*.el" "*.txt") nil)
  ;; (search-gen-find-filter '("*.el" "*.txt") '(".git" ".svn"))
  ;; (search-gen-find-filter nil '(".git" ".svn"))
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  (let (icmd xcmd)
    (setq exclude (append exclude search-ignored-paths-for-find-command))
    (ignore-errors
      (when include
        (mapc (lambda (exp)
                (setq icmd (concat icmd " -path \"" exp "\"")))
              include))
      (when exclude
        (mapc (lambda (exp)
                (setq xcmd (concat xcmd " -not -path \"" exp "\"")))
              exclude)))
    (concat icmd xcmd)))

(defun search-grep-backend (args)
  "[internal use]
Search thing by using GREP."
  (let* ((match (plist-get args :match))
         (dirs (plist-get args :dirs))
         (include (nth 0 dirs))
         (exclude (nth 1 dirs))
         (real-dirs (cddr dirs))
         (files (plist-get args :files))
         (fromfile (plist-get args :fromfile)))
    (eval
     `(search:chain
       ;; Prepare input file.
       ;; FILES part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ,(when files
          `(search:lambda
             (with-temp-file search-temp-file
               (mapc (lambda (path)
                       (and (file-exists-p path)
                            (insert (expand-file-name path) "\n")))
                     ,files))))
       ;; DIRS part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ,@(mapcar (lambda (path)
                   `(search:process-shell-to-file
                     ,(format "find %s %s 2>/dev/null"
                              (expand-file-name path)
                              (search-gen-find-filter include exclude))
                     search-temp-file))
                 real-dirs)
       ;; FROMFILE part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ,(when fromfile
          `(search:lambda
             (with-current-buffer (find-file-noselect search-temp-file)
               (insert "\n")
               (insert-file-contents-literally ,fromfile))))
       ;; Start to search by using input file.
       (search:process-shell-to-search-buffer
        ,(format "xargs grep -nH -e \"%s\" <\"%s\" 2>/dev/null"
                 match
                 (expand-file-name search-temp-file)))))))

(defun search-gen-ack-filter (include exclude)
  "[internal use]
Take INCLUDE and EXCLUDE arguments and generate ACK command string. The format
depends on ACK's option, --type-set=include:ext:??? for include; 
--type-set=exclude:ext:???."
  ;; Sample ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; (search-gen-ack-filter nil nil)
  ;; (search-gen-ack-filter '("el" "txt" "md") nil)
  ;; (search-gen-ack-filter nil '(".git" ".svn"))
  ;; (search-gen-ack-filter '("el" "txt") '(".git" ".svn"))
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  (let (icmd xcmd)
    (ignore-errors
      (when include
        (setq icmd " --type-set=include:ext:")
        (mapc (lambda (exp)
                (setq icmd (concat icmd exp ",")))
              include)
        (setq icmd (replace-regexp-in-string ",$" "" icmd)
              icmd (concat icmd " --type=include")))
      (when exclude
        (setq xcmd " --type-set=exclude:ext:")
        (mapc (lambda (exp)
                (setq xcmd (concat xcmd exp ",")))
              exclude)
        (setq xcmd (replace-regexp-in-string ",$" "" xcmd)
              xcmd (concat xcmd " --type=noexclude"))))
    (concat icmd xcmd)))

(defun search-ack-backend (args)
  "[internal use]
Search thing by using ACK."
  (let* ((match (plist-get args :match))
         (dirs (plist-get args :dirs))
         (include (nth 0 dirs))
         (exclude (nth 1 dirs))
         (real-dirs (cddr dirs))
         (files (plist-get args :files))
         (fromfile (plist-get args :fromfile)))
    (eval
     `(search:chain
       ;; Prepare input file.
       ;; FILES part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ,(when files
          `(search:lambda
             (with-temp-file search-temp-file
               (mapc (lambda (path)
                       (insert path "\n"))
                     ,files))))
       ;; DIRS part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; ,(when dirs
       ;;    `(search:lambda
       ;;       (with-current-buffer (find-file-noselect search-temp-file)
       ;;         (mapc (lambda (path)
       ;;                 (and (file-exists-p path)
       ;;                      (insert (expand-file-name path) "\n")))
       ;;               ,dirs)))
       ;;    `(search:process-shell-to-file
       ;;      ,(format "xargs ack -f %s <%s"
       ;;               (search-gen-ack-filter include exclude)
       ;;               (expand-file-name search-temp-file))
       ;;      search-temp-file))
       ;; TODO: improve speed
       ,@(mapcar (lambda (path)
                   `(search:process-shell-to-file
                     ,(format "ack -f %s %s"
                              (search-gen-ack-filter include exclude)
                              (expand-file-name path))
                     search-temp-file))
                 real-dirs)
       ;; FROMFILE part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ,(when fromfile
          `(search:lambda
             (with-current-buffer (find-file-noselect search-temp-file)
               (insert "\n")
               (insert-file-contents-literally ,fromfile))))
       ;; Start to search by using input file.
       (search:process-shell-to-search-buffer
        ,(format "xargs ack --nocolor \"%s\" <\"%s\" 2>/dev/null"
                 match
                 (expand-file-name search-temp-file)))))))

(defun search-ag-backend (args)
  "[internal use]
Search thing by using AG."
  (let* ((match (plist-get args :match))
         (dirs (plist-get args :dirs))
         (include (nth 0 dirs))
         (exclude (nth 1 dirs))
         (real-dirs (cddr dirs))
         (files (plist-get args :files))
         (fromfile (plist-get args :fromfile)))
    (eval
     `(search:chain
       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; :match      REGEXP or simple string
;; :dirs       '(INCLUDE_LIST EXCLUDE_LIST PATH1 PATH2 PATH3 ...)
;; :files      '(PATH1 PATH2 PATH3 ...)
;; :fromfile   PATH
;;
;; (search-string "def" :dirs '(("*.el") ("*.git*" "*.svn*") "/Users/boyw165/.emacs.d/oops"))
;;;###autoload
(defun search-string (match &rest args)
  "Search MATCH ..."
  ;; Sample ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; (search-string "def" :dirs '(nil ("*.git*" "*.svn*") "/usr/include"))
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  (interactive
   (let* ((match (read-from-minibuffer "Search: "))
          (path (expand-file-name
                 ;; TODO: read file or directory name.
                 ;; TODO: history.
                 (ido-read-directory-name
                  (format "Search %s in: " match)))))
     (cond
      ((file-regular-p path)
       (list match :files `(,path)))
      ((file-directory-p path)
       (list match :dirs `(nil nil ,path))))))
  (search-exec?)
  (when (stringp match)
    (if (< search-tasks-count search-tasks-max)
        (eval
         `(progn
            ;; Increase counter.
            (setq search-tasks-count (1+ search-tasks-count))
            (search-prepare-search-buffer)
            (funcall search-start-action-function)
            ;; Start searching.
            (search:chain
             search-clean-temp-file-task
             ;; Print opened delimiter.
             (search:lambda-to-search-buffer
               (goto-char (point-max))
               (insert (car search-delimiter) ,match "\n")))
            ;; Delegate to `search-backends' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            (funcall (nth 3 search-backends)
                     ',(append (list :match match) args))
            ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            (search:chain
             search-clean-temp-file-task
             search-print-closed-delimiter)))
      (message
       "Search string, \"%s\", is denied due to full queue."
       match))
    search-tasks-count))

;;;###autoload
(defun search-string-command (cmd)
  "Take CMD arguement and execute it asynchronously."
  (interactive
   (list (read-from-minibuffer
          "Search Command: "
          (nth 2 search-backends))))
  ;; Redirect error to null device.
  (setq cmd (concat cmd " 2>/dev/null"))
  (search-exec?)
  (search-prepare-search-buffer)
  (funcall search-start-action-function)
  ;; Start searching.
  (search:chain
   ;; Print opened delimiter.
   (eval
    `(search:lambda-to-search-buffer
       (goto-char (point-max))
       (insert (car search-delimiter) ,cmd "\n")))
   ;; Delegate to `search-backends' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   (search:process-shell-to-search-buffer cmd)
   ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   search-print-closed-delimiter))

;;;###autoload
(defun search-toggle-search-result ()
  (interactive)
  (if (string= (buffer-name) search-buffer-name)
      ;; Hide search buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (if (search-running?)
          ;; TODO: Hide buffer instead of killing it.
          ()
        (and (buffer-modified-p)
             (save-buffer))
        (kill-buffer))
    ;; Show search buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (if (get-buffer search-buffer-name)
        (switch-to-buffer search-buffer-name)
      (find-file search-saved-file)
      (rename-buffer search-buffer-name))))

;;;###autoload
(defun search-stop (index)
  "Stop search task of INDEX index."
  (interactive '(0))
  (if (featurep 'helm)
      (progn
        (message "feature constructing..."))
    (message "helm package is necessary but you don't have it installed.")))

;;;###autoload
(defun search-stop-all ()
  "Stop all search tasks."
  (interactive)
  ;; Kill asynchronous tasks and let synchronous tasks continue.
  (dolist (task search-tasks)
    (and (plist-get task :async)
         (setq search-tasks (delq task search-tasks))))
  ;; Stop async process.
  (when (process-live-p search-proc)
    (setq search-proc (delete-process search-proc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major Mode for Search Result ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'search-result-mode)

(provide 'search)
;;; search.el ends here
