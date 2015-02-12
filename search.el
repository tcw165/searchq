;;; search.el --- Framework of queued search tasks. Support GREP, ACK, AG and more.
;;
;; Copyright (C) 2014-2015
;;
;; Author: boyw165
;; Version: 20150209.1800
;; Package-Requires: ((emacs "24.3") (hl-anything "1.0.0"))
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
;; TODO
;; ----
;; * Change the way of interaction of `search-string'.
;; * Cancel individual search task.
;; * `search-toggle-search-result' shouldn't always kill search buffer.
;; * Add menu items and toolbar buttons.
;; * Open with search-result will cause hl-highlight-mode work incorrectly.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2015-02-13
;; * Initial release.
;; * Support queued search task.
;; * Support both of asynchronous process and synchronous script task.
;; * Support imenu for search result.
;;
;;; Code:

;; GNU library.
(require 'ido)

;; 3rd party libary.
(require 'hl-anything)

(defgroup search nil
  "Search")

(defconst search-default-backends '(("grep" . search-grep-backend)
                                    ("ack" . search-ack-backend)
                                    ("ag" . search-ag-backend))
  "Default alist of search backends.")

(defconst search-buffer-name "*Search Result*"
  "Search buffer name.")

(defconst search-temp-buffer-name "*Search Temp*"
  "Temporary search buffer name.")

(defcustom search-backends (nth 0 search-default-backends)
  "Search tool name. Default is GREP."
  :type `(choice ,@(mapcar (lambda (c)
                             `(const :tag ,(car c) ,(car c) ,(cdr c)))
                           search-default-backends)
                 (cons :tag "user defined"
                       (match :tag "exec name"
                               "dummy")
                       (function :tag "command generator function"
                                 search-dummy-backend)))
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

(defcustom search-prompt-function 'search-default-prompt-function
  "Prompt function."
  :type 'function
  :group 'search)

(defvar search-tasks nil
  "Search tasks queue.")

(defvar search-tasks-count 0
  "Search tasks count.")

(defvar search-proc nil
  "Search task process.")

(defvar search-timer nil
  "A delay timer for evaluating the queued tasks.")

(defvar search-prompt-timer nil
  "A timer for showing prompt animation.")

(defun search-exec? ()
  "[internal usage]
Test whether the necessary exe(s) are present."
  (unless (and (executable-find "sh")
               (executable-find "find")
               (executable-find "xargs")
               (executable-find (car search-backends)))
    (error "%s or xargs is not supported on your system!" (car search-backends)))
  t)

(defun search-running? ()
  "[internal usage]
Test whether the search is under processing."
  (> search-tasks-count 0))

(defmacro search-with-search-buffer (&rest body)
  "[internal usage]
Evaluate BODY in the search buffer."
  (declare (indent 0) (debug t))
  `(with-current-buffer (get-buffer-create search-buffer-name)
     (set-auto-mode t)
     (setq buffer-file-name (expand-file-name search-saved-file))
     ,@body))

(defun search-create-task (func async)
  "[internal usage]
Create a search object with FUNC function and ASYNC boolean."
  (list :func func
        :async async))

(defun search-task-p (task)
  "[internal usage]
Test if the TASK is a valid search object."
  (and (plist-get task :func)
       (booleanp (plist-get task :async))))

(defun search-append-task (task)
  "[internal usage]
Append TASK to `search-tasks' and evaluate it later. See `search-create-task'."
  (when (search-task-p task)
    (setq search-tasks (delq search-last search-tasks)
          search-tasks (append search-tasks (list task search-last)))))

(defun search-doer ()
  "[internal usage]
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
    (if search-tasks
        (cond
         ((null (plist-get task :async))
          (search-setup-doer)))
      ;; Clean the timer if there's no task.
      (and (timerp search-timer)
           (setq search-timer (cancel-timer search-timer)))
      ;; Stop async process.
      (and (process-live-p search-proc)
           (setq search-proc (delete-process search-proc)))
      ;; Stop prmopt.
      (search-stop-prompt))))

(defun search-setup-doer ()
  "[internal usage]
Run `search-doer' after a tiny delay."
  (and (timerp search-timer)
       (cancel-timer search-timer))
  (setq search-timer (run-with-idle-timer
                      search-timer-delay nil
                      'search-doer)))

(defun search-start-dequeue ()
  "[internal usage]
Start to evaluate search task in the queue."
  (unless (timerp search-timer)
    ;; Setup timer
    (search-setup-doer)
    ;; Start prmopt.
    (search-start-prompt)))

(defvar search-prompt-animation '("-" "\\" "|" "/")
  "[internal usage]
Prompt animation.")

(defun search-message (message &rest args)
  "[internal usage]
Display MESSAGE in the minibuffer when minibuffer is inactive."
  (when (not (minibufferp (current-buffer)))
    (if args
        (apply 'message message args)
      (message "%s" message))))

(defun search-default-prompt-function ()
  "[internal usage]
Default prompt function."
  (let ((char (car search-prompt-animation)))
    (search-message "Search ...%s" char)
    (setq search-prompt-animation (cdr search-prompt-animation)
          search-prompt-animation (append
                                   search-prompt-animation
                                   (list char)))))

(defun search-start-prompt ()
  "[internal usage]
Start prmopt animation."
  (unless (timerp search-prompt-timer)
    (setq search-prompt-timer
          (run-with-timer
           0 0.1
           (lambda ()
             (and (functionp search-prompt-function)
                  (funcall search-prompt-function)))))))

(defun search-stop-prompt ()
  "[internal usage]
Stop prompt animation."
  (when (timerp search-prompt-timer)
    (setq search-prompt-timer (cancel-timer search-prompt-timer)))
  (search-message "Search ...done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Task API for Backends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun search:chain (&rest tasks)
  "Chain the search tasks."
  (mapc 'search-append-task tasks)
  (search-start-dequeue)
  nil)

(defun search:lambda (func)
  "Create a search object wrapping FUNC which is a lambda function."
  (search-create-task
   ;; Function.
   func
   ;; Synchronous.
   nil))

(defun search:lambda-to-search-buffer (func)
  "Create a search object wrapping FUNC under `search-buffer'."
  (search:lambda
   (eval
    `(lambda ()
       ,(and (functionp func)
             `(search-with-search-buffer
                (condition-case err
                    (save-excursion
                      (funcall ,func))
                  (error (message "search:lambda-to-search-buffer | error: %s"
                                  (error-message-string err))))))))))

(defun search:process-shell (command bufname &optional callback)
  "Create a search object wrapping `start-process-shell-command' with COMMAND.
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
  "Create a search object wrapping `start-process-shell-command' with COMMAND.
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
  "Create a search object wrapping `start-process-shell-command' with COMMAND.
The output will be dumpped directly to the `search-buffer'."
  (search:process-shell
    command search-buffer-name
    (lambda ()
      (setq buffer-file-name (expand-file-name search-saved-file)))))

;; !important! The last search object.
(defvar search-last (search:lambda
                     (lambda ()
                       ;; Stop prompt.
                       (search-stop-prompt)
                       ;; Reset counter.
                       (setq search-tasks-count 0)))
  "[internal usage]
The search object which always being the last one.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun search-dummy-backend (&rest args)
  "[internal usage]
Just a dummy backend. See `search-thing'.")

(defun search-gen-find-filter (include exclude)
  "[internal usage]
Take INCLUDE and EXCLUDE arguments and generate FIND command string. The format
depends on the tool you use.
e.g. *.txt is for GREP command."
  ;; Sample ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; (search-gen-find-filter nil nil)
  ;; (search-gen-find-filter '("*.el" "*.txt") nil)
  ;; (search-gen-find-filter '("*.el" "*.txt") '(".git" ".svn"))
  ;; (search-gen-find-filter nil '(".git" ".svn"))
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  (let (icmd xcmd)
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
  "[internal usage]
Search thing by using GREP."
  (let* ((cmd (plist-get args :cmd))
         (match (plist-get args :match))
         (dirs (plist-get args :dirs))
         (include (nth 0 dirs))
         (exclude (nth 1 dirs))
         (real-dirs (cddr dirs))
         (files (plist-get args :files))
         (fromfile (plist-get args :fromfile)))
    (if cmd
        (eval
         `(search:chain
           (search:process-shell-to-search-buffer ,cmd)))
      (eval
       `(search:chain
         ;; Prepare input file.
         ;; FILES part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ,(when files
            `(search:lambda
              (lambda ()
                (with-temp-file search-temp-file
                  ,(cond
                    ((stringp files) `(insert ,files))
                    ((listp files) `(mapc (lambda (str)
                                            (insert str "\n"))
                                          ,files)))))))
         ;; DIRS part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ,@(mapcar (lambda (path)
                     `(search:process-shell-to-file
                       ,(format "find %s %s"
                                (expand-file-name path)
                                (search-gen-find-filter include exclude))
                       search-temp-file))
                   real-dirs)
         ;; FROMFILE part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ,(when fromfile
            `(search:lambda
              (lambda ()
                (with-current-buffer (find-file-noselect search-temp-file)
                  (insert "\n")
                  (insert-file-contents-literally ,fromfile)))))
         ;; Start to search by using input file.
         (search:process-shell-to-search-buffer
          ,(format "xargs grep -nH \"%s\" <\"%s\" 2>/dev/null"
                   match
                   (expand-file-name search-temp-file))))))))

(defun search-ack-backend (args)
  "[internal usage]
Search thing by using ACK."
  (let* ((cmd (plist-get args :cmd))
         (match (plist-get args :match))
         (dirs (plist-get args :dirs))
         (include (nth 0 dirs))
         (exclude (nth 1 dirs))
         (real-dirs (cddr dirs))
         (files (plist-get args :files))
         (fromfile (plist-get args :fromfile)))
    ))

(defun search-ag-backend (args)
  "[internal usage]
Search thing by using AG."
  (let* ((cmd (plist-get args :cmd))
         (match (plist-get args :match))
         (dirs (plist-get args :dirs))
         (include (nth 0 dirs))
         (exclude (nth 1 dirs))
         (real-dirs (cddr dirs))
         (files (plist-get args :files))
         (fromfile (plist-get args :fromfile)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun search-stop ()
  (interactive)
  ;; Stop prompt.
  (search-stop-prompt)
  ;; Stop timer
  (when (timerp search-timer)
    (setq search-timer (cancel-timer search-timer)))
  ;; Stop async process.
  (when (process-live-p search-proc)
    (setq search-proc (delete-process search-proc)))
  ;; Clear tasks.
  (setq search-tasks nil
        search-tasks-count 0))

;; :cmd        COMMAND string (top priority)
;; :match      REGEXP or simple string
;; :dirs       '(INCLUDE_LIST EXCLUDE_LIST PATH1 PATH2 PATH3 ...)
;; :files      '(PATH1 PATH2 PATH3 ...)
;; :fromfile   PATH
;;
;; (search-string "def" :dirs '(("*.el") ("*.git*" "*.svn*") "/Users/boyw165/.emacs.d/oops"))
;;;###autoload
(defun search-string (match &rest args)
  "ARGS format:
  :dirs       '(INCLUDE_LIST EXCLUDE_LIST PATH1 PATH2 PATH3 ...)
  :files      '(PATH1 PATH2 PATH3 ...)
  :fromfile   PATH"
  ;; Sample ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; (search-string "def" :dirs '(nil ("*.git*" "*.svn*") "/path/a" "/path/b"))
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  (interactive
   (let* ((file (buffer-file-name))
          (dir (file-name-directory file))
          (ans (ido-completing-read
                "Search file or directory? "
                `("file" "dir") nil t))
          (args (cond
                 ((string-match "^file" ans)
                  (list :files file))
                 ((string-match "^dir" ans)
                  (list :dirs dir)))))
     (push (read-from-minibuffer "Search: ") args)))
  (search-exec?)
  (when (stringp match)
    (if (< search-tasks-count search-tasks-max)
        (progn
          ;; Increase counter.
          (setq search-tasks-count (1+ search-tasks-count))
          ;; Switch to search buffer.
          (switch-to-buffer (search-with-search-buffer
                              (and (file-exists-p search-saved-file)
                                   (insert-file-contents-literally
                                    search-saved-file
                                    nil nil nil t))
                              (current-buffer)))
          (search:chain
           ;; Delete temp file.
           (search:lambda
            (lambda ()
              (and (file-exists-p search-temp-file)
                   (delete-file search-temp-file))))
           ;; Print opened delimiter.
           (search:lambda-to-search-buffer
            (eval
             `(lambda ()
                (goto-char (point-max))
                (insert ,(car search-delimiter) ,match "\n")))))
          ;; Delegate to `search-backends' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (funcall (cdr search-backends)
                   (append (list :match match) args))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (search:chain
           ;; Delete intermidiate file.
           (search:lambda
            (lambda ()
              (and (file-exists-p search-temp-file)
                   (delete-file search-temp-file))))
           ;; Print closed delimiter.
           (search:lambda-to-search-buffer
            (eval
             `(lambda ()
                (goto-char (point-max))
                (insert ,(cdr search-delimiter) "\n\n")
                (save-buffer))))))
      (message
       "Search string, \"%s\", is denied due to full queue."
       match))
    search-tasks-count))

;;;###autoload
(defun search-string-command (cmd)
  (interactive
   ;; TODO:
   )
  (funcall (cdr search-backends) cmd))

;;;###autoload
(defun search-toggle-search-result ()
  (interactive)
  (if (string= (buffer-name) search-buffer-name)
      ;; TODO: Kill buffer without asking.
      (progn
        (kill-buffer)
        ;; (mapc (lambda (win)
        ;;         (when (window-live-p win)
        ;;           ))
        ;;       (window-list))
        )
    (find-file search-saved-file)
    (rename-buffer search-buffer-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major Mode for Search Result ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'search-result-mode)

(provide 'search)
;;; search.el ends here
