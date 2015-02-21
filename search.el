;;; search.el --- Framework of queued search tasks using GREP, ACK, AG and more.
;;
;; Copyright (C) 2014-2015
;;
;; Author: boyw165
;; Version: 20150219.1800
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
;; * Support AG.
;; * Improve interaction of `search-thing' and `search-thing-command'.
;; * Cancel individual search task.
;; * Open with search-result will cause hl-highlight-mode work incorrectly.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2015-02-19
;; * Initial release.
;; * Support queued search task.
;; * Support both of asynchronous process and synchronous script task.
;; * Support killing asynchronous search task.
;; * Support imenu for search result.
;;
;;; Code:

;; GNU library.
(require 'font-lock)
(require 'hl-line)
(require 'ido)
(require 'imenu)
(require 'saveplace)

;; 3rd party libary.
(require 'hl-anything)

(defgroup search nil
  "Search")

(defconst search-default-backends
  '(("FIND and GREP" ("find" "grep") "find $pwd|xargs grep -nH -e \"$0\" 2>/dev/null"
     search-grep-backend)
    ("ACK only"      ("ack")         "ack --nocolor '$0' $pwd 2>/dev/null"
     search-ack-backend)
    ("AG only"       ("ag")          "echo constructing..."
     search-ag-backend))
  "Default search backends. The format:
* The 1st element is description string.
* The 2nd element is a exec path list to be tested.
* The 3rd element is a command template string.
  $0 is the cursor position; $pwd is the current directory.
* The 4th element is the backend which is the doer of everything.")

(defconst search-buffer-name "Search-Result"
  "Search buffer name.")

(defconst search-temp-buffer-name "*Search-Temp*"
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
  "Temporary file for search. e.g. as an input file with context of listed 
files."
  :type 'string
  :group 'search)

(defcustom search-tasks-max 5
  "Maximum length of the task queue."
  :type 'integer
  :group 'search)

(defcustom search-timer-delay 0.3
  "Delay seconds for every search task. Try don't make it less than 0.3."
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
    (setq search-tasks-count (1- search-tasks-count))
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
depends on the FIND's option, --path."
  ;; Sample ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; (search-gen-find-filter nil nil)
  ;; (search-gen-find-filter '("*.el" "*.txt") nil)
  ;; (search-gen-find-filter '("*.el" "*.txt") '(".git" ".svn"))
  ;; (search-gen-find-filter nil '(".git" ".svn"))
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  (ignore-errors
    (let (icmd xcmd)
      (setq exclude (append exclude search-ignored-paths-for-find-command))
      
      (when include
        (mapc (lambda (exp)
                (setq icmd (concat icmd " -path \"" exp "\"")))
              include))
      (when exclude
        (mapc (lambda (exp)
                (setq xcmd (concat xcmd " -not -path \"" exp "\"")))
              exclude))
      (concat icmd xcmd))))

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
depends on ACK's option,
--type-set=include:ext:??? for includes; 
--type-set=exclude:ext:??? for excludes."
  ;; Sample ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; (search-gen-ack-filter nil nil)
  ;; (search-gen-ack-filter '("el" "txt" "md") nil)
  ;; (search-gen-ack-filter nil '("git" "svn"))
  ;; (search-gen-ack-filter '("el" "txt") '("git" "svn"))
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  (ignore-errors
    (let (icmd xcmd)
      (when include
        (setq icmd " --type-set=include:ext:")
        (mapc (lambda (exp)
                (setq icmd (concat icmd exp ",")))
              include)
        (setq icmd (progn
                     (string-match "\\(.*\\),$" icmd)
                     (match-string 1 icmd))
              icmd (concat icmd " --type=include")))
      (when exclude
        (setq xcmd " --type-set=exclude:ext:")
        (mapc (lambda (exp)
                (setq xcmd (concat xcmd exp ",")))
              exclude)
        (setq xcmd (progn
                     (string-match "\\(.*\\),$" xcmd)
                     (match-string 1 xcmd))
              xcmd (concat xcmd " --type=noexclude")))
      (concat icmd xcmd))))

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

;;;###autoload
(defun search-thing (match &rest args)
  "Make a search task to search MATCH string or regular expression refer to 
attributes ARGS. ARGS describes what files, or what directories to search.
Search tasks is delegated to `search-backends'.

Format of ARGS
--------------
* Search MATCH in specific files:
  ARGS = :files '(filename1 filename2 filename3 ...)
* Search MATCH in specific directories:
  ARGS = :dirs '(INCLUDES EXCLUDES dirpath1 dirpath2 dirpath3 ...)
  INCLUDES = A filter list for including files.
             ex: '(*.md *.txt) for `search-grep-backend'.
  EXCLUDES = A filter list for excluding files.
             ex: '(*.git* *.svn*) for `search-grep-backend'.
  !Note: The format of INCLUDES and EXCLUDES depends on the backend you're using.
* Search MATCH in files listed in an input file.
  ARGS = :fromfile filename

Example
-------
* Search MATCH in specific files and directories.
  (search-thing MATCH :files '(/path/a /path/b) :dirs '(nil nil /path/dir1 /path/dir2))
* Search MATCH in files listed in an input file.
  (search-thing MATCH :fromfile /path/inputfile)
* Search MATCH in specific directories and ignore subversion files.
  (search-thing MATCH :dirs '(nil (*.git* *.svn*) /path/dir1 /path/dir2))
"
  (interactive
   (let* ((match (read-from-minibuffer
                  "Search: "
                  (let ((bounds (bounds-of-thing-at-point 'symbol)))
                    (and bounds (buffer-substring-no-properties (car bounds)
                                                                (cdr bounds))))))
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
    ;; Return tasks count.
    search-tasks-count))

;;;###autoload
(defun search-thing-command (cmd)
  "Very similar to `search-thing' but it takes CMD arguement and pass it to 
`search-backends' directly."
  (interactive
   (let* ((prompt "Search Command: ")
          (cmd (replace-regexp-in-string
                "\\$pwd" (concat "\"" default-directory "\"")
                (nth 2 search-backends)))
          (pos (string-match "\\$0" cmd)))
     (minibuffer-with-setup-hook
         (eval
          `(lambda ()
             (goto-char ,(+ (length prompt) pos 1))))
       (list (read-from-minibuffer
              prompt
              (concat (substring cmd 0 pos)
                      (substring cmd (match-end 0))))))))
  ;; Try to get thing to be searched.
  (let ((thing (let* ((cmd-sample (replace-regexp-in-string
                                   "\\$pwd" (concat "\"" default-directory "\"")
                                   (nth 2 search-backends)))
                      (regexp (replace-regexp-in-string
                               "\\\\\\$0" "\\\\(.*\\\\)"
                               (regexp-quote cmd-sample))))
                 (and (string-match regexp cmd)
                      (match-string 1 cmd)))))
    (search-exec?)
    (search-prepare-search-buffer)
    (funcall search-start-action-function)
    ;; Start searching.
    (search:chain
     ;; Print opened delimiter.
     (eval
      `(search:lambda-to-search-buffer
         (goto-char (point-max))
         (insert (car search-delimiter) ,(or thing cmd) "\n")))
     ;; Delegate to `search-backends' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     (search:process-shell-to-search-buffer cmd)
     ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     search-print-closed-delimiter)))

;;;###autoload
(defun search-toggle-search-result ()
  (interactive)
  (if (string= (buffer-name) search-buffer-name)
      ;; Hide search buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (progn
        (and (buffer-modified-p)
             (save-buffer))
        (switch-to-prev-buffer))
    ;; Show search buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (if (get-buffer search-buffer-name)
        (switch-to-buffer search-buffer-name)
      ;; `search-result-mode' is applied automatically.
      (find-file search-saved-file))))

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

(defgroup search-result nil
  "Project result mode for .result file."
  :group 'search)

(defcustom search-result-mode-hook `(save-place-find-file-hook
                                     font-lock-mode
                                     linum-mode
                                     hl-line-mode)
  "Hook run when entering `search-result-mode' mode."
  :type 'hook
  :group 'search-result)

(defvar search-result-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    ;; (define-key map [up] )
    ;; (define-key map [down] )
    (define-key map [return] 'search-result-find-file)
    (define-key map [?q] 'search-toggle-search-result)
    (define-key map [escape] 'search-toggle-search-result)
    (define-key map [?d] 'search-result-delete-item-atpt)
    map)
  "[internal use]
Keymap for `search-result-mode'.")

(defvar search-result-mode-font-lock-keywords
  `((;; Delimiter and match string.
     (,(format "^%s\\(.*\\)" (regexp-quote (car search-delimiter))) (1 'search-highlight-face))
     ;; GREP style.
     ("^\\([[:alnum:] $_\/.+-]+\\):\\([0-9]+\\):" (1 'search-file-face) (2 'search-linum-face))
     ;; ACK style.
     ("^\\([a-Z]:\\\\\\|~/\\|/\\).*$" . 'search-file-face)
     ("^\\([0-9]+\\):" (1 'search-linum-face))
     ;; TODO: AG style.
     )
    ;; don't use syntactic fontification.
    t
    ;; Case insensitive.
    nil)
  "[internal use]
Font lock keywords for `search-result-mode'. See `font-lock-defaults' and 
`font-lock-keywords'.")

(defun search-imenu-create-index ()
  "[internal use]
Return imenu index for `search-result-mode'. See `imenu--index-alist' for the 
format of the buffer index alist."
  ;; (when (and (string= (buffer-name) search-buffer-name)
  ;;            (not (search-running?)))
  (when (string= (buffer-name) search-buffer-name)
    (let (index)
      (save-excursion
        (goto-char (point-max))
        (while (re-search-backward
                (concat "^" (regexp-quote (car search-delimiter)) "\\(.*\\)$")
                nil t)
          (push (cons (match-string-no-properties 1)
                      (line-end-position)) index))
        (list (cons "Search Task" index))))))

(defun search-result-is-valid-item ()
  "[internal use]
Test valid item at point."
  (save-excursion
    (beginning-of-line)
    (not (or (looking-at (regexp-quote (car search-delimiter)))
             (looking-at (regexp-quote (cdr search-delimiter)))
             (looking-at "$")))))

(defun search-result-clean-empty-item ()
  "[internal use]
Delete invalid item."
  (save-excursion
    (goto-char 1)
    (while (re-search-forward (format "%s.*[\n\r]%s"
                                      (regexp-quote (car search-delimiter))
                                      (regexp-quote (cdr search-delimiter)))
                              nil t)
      (goto-char (match-beginning 0))
      (delete-region (line-beginning-position 1)
                     (line-beginning-position 4)))))

(defun search-result-delete-item-atpt ()
  "[internal use]
Delete item at point."
  (interactive)
  (if mark-active
      (let ((end-mark (set-marker (copy-marker (mark-marker) t) (region-end))))
        ;; TODO: Large region slows very much.
        (goto-char (region-beginning))
        (beginning-of-line)
        (setq mark-active nil)
        (while (< (point) (marker-position end-mark))
          (if (search-result-is-valid-item)
              (delete-region (line-beginning-position 1)
                             (line-beginning-position 2))
            (forward-line)))
        (set-marker end-mark nil))
    (and (search-result-is-valid-item)
         (delete-region (line-beginning-position 1)
                        (line-beginning-position 2))))
  (and (buffer-modified-p) (save-buffer)))

(defun search-result-find-file ()
  "[internal use]
Open search item."
  (interactive)
  (let (file linum)
    (save-excursion
      (beginning-of-line)
      (cond
       ;; GREP style ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ((looking-at "^\\(.+\\):\\([0-9]+\\):")
        (setq file (match-string 1)
              linum (string-to-number (match-string 2))))
       ;; ACK style ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (
        )))
    ;; Open file if any.
    (when (file-exists-p file)
      (find-file file)
      (goto-char 1)
      (forward-line (1- linum))
      (end-of-line)
      (recenter 3))))

;;;###autoload
(define-derived-mode search-result-mode nil "Search-Result"
  "Major mode for search buffers."
  :group 'result-group
  (remove-overlays)
  (setq font-lock-defaults search-result-mode-font-lock-keywords
        truncate-lines t)
  ;; Set local imenu generator.
  (setq-local imenu-create-index-function 'search-imenu-create-index)
  ;; Rename buffer to `search-buffer-name'
  (rename-buffer search-buffer-name)
  (add-hook 'before-save-hook 'search-result-clean-empty-item nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup search-result-face nil
  "Additional faces for `hl-anything'."
  :group 'search-result)

(defface search-file-face
  '((t (:foreground "blue" :underline t :weight bold)))
  "Default face for file path. Suggest no background, which will be overridden
by `hl-line-mode' or `global-hl-line-mode'."
  :group 'search-result-face)

(defface search-linum-face
  '((t (:foreground "maroon1")))
  "Default face for linum number. Suggest no background, which will be overridden
by `hl-line-mode' or `global-hl-line-mode'."
  :group 'search-result-face)

(defface search-highlight-face
  '((t (:foreground "gold" :background "black" :weight bold :height 1.3)))
  "Default face for highlighting keyword in definition window."
  :group 'search-result-face)

;; Add faces to `hl-highlight-special-faces'.
(add-to-list 'hl-highlight-special-faces 'search-highlight-face)

;; Integrate with `history' if any.
(when (featurep 'history)
  (add-to-list 'history-advised-before-functions 'search-result-find-file)
  (add-to-list 'history-advised-after-functions 'search-result-find-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Menu & Toolbar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Menu items.
  ;; Tool-bar buttons.
(when tool-bar-mode
  (define-key-after tool-bar-map [search-thing]
    '(menu-item "Search Thing" search-thing
                :image (find-image '((:type xpm :file "images/search-thing.xpm"))))))

(provide 'search)
;;; search.el ends here
