;;; search.el --- Framework of queued search tasks. Support GREP, ACK, AG and more.
;;
;; Copyright (C) 2015
;;
;; Author: boyw165
;; Version: 20150115.1500
;; Package-Requires: ((emacs "24.3"))
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
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2015-02-05
;; * Initial release.
;;
;;; Code:

;; GNU library.
(require 'ido)

;; 3rd party libary.
(require 'deferred)

(defgroup search nil
  "Search")

(defconst search-default-backends '(("grep" . search-grep-backend)
                                    ;; ("ack" . search-ack-backend)
                                    ;; ("ag" . search-ag-backend)
                                    )
  "Default alist of search backends.")

(defconst search-buffer-name "*Search Result*"
  "Search buffer name.")

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

(defcustom search-print-function 'search-default-print
  "Function of printing search result."
  :type 'function
  :group 'search)

(defcustom search-saved-file (expand-file-name "~/.emacs.d/.search")
  "File for cached search result."
  :type 'string
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

(defcustom search-delimiter '(">>>>>" . "<<<<<")
  "Maximum length of the task queue."
  :type '(cons (match :tag "open delimiter")
               (match :tag "close delimiter"))
  :group 'search)

(defvar search-tasks nil
  "Search tasks queue.")

(defvar search-timer nil
  "A delay timer for evaluating the queued tasks.")

(defvar search-prompt-timer nil
  "A timer for showing prompt animation.")

(defvar search-prompt-animation '("-" "\\" "|" "/")
  "Prompt animation.")

(defun search-exec? ()
  "Test whether the necessary exe(s) are present."
  (unless (and (executable-find "sh")
               (executable-find "find")
               (executable-find "xargs")
               (executable-find (car search-backends)))
    (error "%s or xargs is not supported on your system!" (car search-backends))))

(defun search-buffer ()
  "Get search buffer and refresh its content."
  (with-current-buffer (get-buffer-create search-buffer-name)
    (setq buffer-file-name (expand-file-name search-saved-file))
    (current-buffer)))

;; (defmacro search-with-tmpfile (filename &rest body)
;;   "Append context produced by BODY to the FILENAME file."
;;   (declare (indent 1) (debug t))
;;   `(when (file-writable-p ,filename)
;;      (with-temp-file ,filename
;;        (and (file-exists-p ,filename)
;;             (insert-file-contents-literally ,filename))
;;        (progn ,@body))))

;; (defun search-default-print (match result)
;;   "Print MATCH and RESULT with delimiters."
;;   (with-current-buffer (search-buffer)
;;     (save-excursion
;;       (goto-char (point-max))
;;       (insert "\n"
;;               (car search-delimiter) " " match "\n"
;;               result
;;               (cdr search-delimiter) "\n")
;;       (save-buffer))))

;; (defun search-seralize-list (thing)
;;   (cond
;;    ;; A list of strings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    ((listp thing)
;;     (let ((it thing)
;;           (space "")
;;           str)
;;       (while it
;;         (ignore-errors
;;           (setq str (concat str
;;                             space
;;                             (car it))))
;;         (setq it (cdr it)
;;               space " "))
;;       str))
;;    ;; A match ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    ((stringp thing)
;;     thing)))

(defun search-create-task (func async)
  "[internal usage]
Create a search object with FUNC function and ASYNC boolean."
  (list :func func
        :async async))

(defun search-task-p (task)
  "[internal usage]
Test if the TASK is a valid search object."
  (and (plist-get task :func)
       (plist-get task :async)))

(defun search-append-task (task)
  "[internal usage]
Append TASK to `search-tasks' and evaluate it later. See `search-create-task'."
  (and (search-task-p task)
       (setq search-tasks (append search-tasks (list task)))))

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

(defun search-start-prompt ()
  "[internal usage]
Start prmopt animation."
  (unless (timerp search-prompt-timer)
    (setq search-prompt-timer
          (run-with-timer
           0 0.1
           (lambda ()
             (let ((char (car search-prompt-animation)))
               (message "Search ...%s" char)
               (setq search-prompt-animation (cdr search-prompt-animation)
                     search-prompt-animation (append
                                              search-prompt-animation
                                              (list char)))))))))

(defun search-stop-prompt ()
  "[internal usage]
Stop prompt animation."
  (when (timerp search-prompt-timer)
    (setq search-prompt-timer (cancel-timer search-prompt-timer)))
  (message "Search ...done"))

;; (search-start-prompt)
;; (search-stop-prompt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Task API for Backends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun search:chain (&rest tasks)
  ;; (declare (indent 0) (debug t))
  (mapc 'search-append-task tasks)
  (search-start-dequeue))

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
       (with-current-buffer (search-buffer)
         (and (functionp func)
              (funcall func)))))))

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
       (let* ((buf (get-buffer-create (or ,bufname
                                          "*search-temp*")))
              (proc (start-process-shell-command
                     "*search-proc*" buf ,command)))
         (set-process-sentinel
          proc
          (lambda (proc event)
            (condition-case err
                (with-current-buffer (process-buffer proc)
                  (and (functionp ,callback)
                       (funcall ,callback)))
              (error (message "search:process-shel error: %s"
                              (error-message-string err))))
            (search-setup-doer))))))
   ;; Asynchronous.
   t))

(defun search:process-shell-to-file (command filename)
  "Create a search object wrapping `start-process-shell-command' with COMMAND.
The output will be written to FILENAME file."
  (search:process-shell
    command (file-name-nondirectory filename)
    (eval `(lambda ()
             (setq buffer-file-name ,filename)
             (save-buffer)
             (kill-buffer)))))

(defun search:process-shell-to-search-buffer (command)
  "Create a search object wrapping `start-process-shell-command' with COMMAND.
The output will be dumpped directly to the `search-buffer'."
  (search:process-shell
    command search-buffer-name
    (lambda ()
      (setq buffer-file-name (expand-file-name search-saved-file))
      (save-buffer))))

;; Test >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(defun test ()
  (search:chain
   (search:process-shell-to-search-buffer "ls -al")
   (search:process-shell-to-search-buffer "ls -al /bin")
   (search:process-shell-to-search-buffer "find /Users/boyw165/.emacs.d/elpa/ -name \"*.el\"|xargs grep -nH def 2>/dev/null")
   (search:process-shell-to-file "ls -al" "/Users/boyw165/.emacs.d/test.txt")))

;; (test)
;; (message "%s" search-tasks)
;; (setq search-tasks nil)
;; (setq search-timer (cancel-timer search-timer))
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun search-ack-backend (match &optional files dirs fromfile filters)
  "Return a deferred object which doing search job with ACK. See `search-thing'."
  )

(defun search-dummy-backend (match &optional files dirs fromfile filters)
  "Return a deferred object which doing nothing. See `search-thing'."
  )

(defun search-grep-backend (match files dirs fromfile filters)
  "Return a deferred object which doing search job with GREP. See `search-thing'."
  (let ((temp-file (concat search-temp-file "."
                           (number-to-string (float-time)))))
    (eval
     `(deferred:$
        ;; Prepare input file.
        ;; `:files' part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; TODO: figure out the difference between `deferred:new' and `deferred:next'.
        (deferred:next
          (lambda ()
            ;; Print files to INPUTFILE line by line.
            (search-with-tmpfile ,temp-file
              (cond
               ((stringp ,files) (insert ,files))
               ((listp ,files) (mapc (lambda (str)
                                       (insert str "\n"))
                                     ,files))))))
        ;; `:dirs' part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ,@(mapcar (lambda (path)
                    `(deferred:nextc it
                       (lambda (&rest ignore)
                         (deferred:nextc
                           (deferred:process-shell
                             ;; TODO: filter
                             ,(format "find %s" (expand-file-name path)))
                           (lambda (result)
                             (search-with-tmpfile ,temp-file
                               (insert result "\n")))))))
                  (cond
                   ((stringp dirs) (list dirs))
                   ((listp dirs) dirs)))
        ;; `:fromfile' part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (deferred:nextc it
          (lambda (&optional x)
            (and ,fromfile
                 (search-with-tmpfile ,temp-file
                   (insert "\n")
                   (insert-file-contents-literally ,fromfile)))))
        ;; Start to search.
        (deferred:nextc it
          (lambda (&rest ignore)
            (deferred:nextc
              (deferred:process-shell
                ,(format "xargs grep -nH \"%s\" <\"%s\" 2>/dev/null || true"
                         match
                         (expand-file-name temp-file)))
              (lambda (result)
                ;; !IMPORTANT! - Print the result.
                ;; Default printer is `search-default-print'.
                (funcall search-print-function ,match result)))))
        ;; Delete intermidiate file.
        (deferred:nextc it
          (lambda (&rest ignore)
            (deferred:process-shell
              ,(format "rm \"%s\" 2>/dev/null" temp-file))))))))

;; (defun search-ag-backend (match &optional filters)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun search-string (&optional match &rest args)
  "FILES format:
  (:files      (1 2 3 ...)
   :dirs       (A B C ...)
   :fromfile   FILE
   :filters    (include . exclude))"
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
    (if (< search-tasks search-tasks-max)
        (progn
          ;; Increase counter.
          (setq search-tasks (1+ search-tasks))
          (deferred:$
            (deferred:earlier
              ;; Delegate to `search-backends'.
              (funcall (cdr search-backends)
                       match
                       (plist-get args :files)
                       (plist-get args :dirs)
                       (plist-get args :fromfile)
                       (plist-get args :filters))
              ;; Animation prompt.
              (deferred:$
                (deferred:next
                  (deferred:lambda (x)
                    (if (= 0 search-tasks)
                        (message "Searching ...done!")
                      (message "Searching...%s" (random 100))
                      (deferred:nextc
                        (deferred:wait 1000)
                        self)))))
              ;; TODO: timeout.
              )
            ;; Decrase counter.
            (deferred:nextc it
              (lambda (&optional x)
                (setq search-tasks (1- search-tasks))))))
      (message
       "Search task, \"%s\", is denied due to full queue. Please wait for a while."
       match))))

;;;###autoload
(defun search-toggle-search-result ()
  (interactive)
  (if (string= buffer-file-name (expand-file-name search-saved-file))
      ;; TODO: Kill buffer without asking.
      (progn
        (kill-buffer)
        ;; (mapc (lambda (win)
        ;;         (when (window-live-p win)
        ;;           ))
        ;;       (window-list))
        )
    (set-window-buffer (selected-window) (search-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major Mode for Search Result ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'search)
;;; search.el ends here
