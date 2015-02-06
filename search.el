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

(defcustom search-result (expand-file-name "~/.emacs.d/.search")
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
  "Search tasks.")

(defvar search-timer nil
  "A delay timer for evaluating the queued tasks.")

(defun search-exec? ()
  "Test whether the necessary exe(s) are present."
  (unless (and (executable-find "sh")
               (executable-find "find")
               (executable-find "xargs")
               (executable-find (car search-backends)))
    (error "%s or xargs is not supported on your system!" (car search-backends))))

(defun search-buffer ()
  "Get search buffer and refresh its content."
  (with-current-buffer (get-buffer-create
                        (file-name-nondirectory search-result))
    (setq buffer-file-name (expand-file-name search-result))
    (current-buffer)))

(defmacro search-with-tmpfile (filename &rest body)
  "Append context produced by BODY to the FILENAME file."
  (declare (indent 1) (debug t))
  `(when (file-writable-p ,filename)
     (with-temp-file ,filename
       (and (file-exists-p ,filename)
            (insert-file-contents-literally ,filename))
       (progn ,@body))))

(defun search-default-print (match result)
  "Print MATCH and RESULT with delimiters."
  (with-current-buffer (search-buffer)
    (save-excursion
      (goto-char (point-max))
      (insert "\n"
              (car search-delimiter) " " match "\n"
              result
              (cdr search-delimiter) "\n")
      (save-buffer))))

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
  (list :func func
        :async async))

(defun search-task-p (task)
  (and (plist-get task :func)
       (plist-get task :async)))

(defun search-append-task (task)
  "Append TASK to `search-tasks' and evaluate it later. See `search-create-task'."
  (setq search-tasks (append search-tasks (list task))))

(defun search-doer ()
  (let ((task (car search-tasks)))
    (pop search-tasks)
    ;; Execute current task.
    (condition-case err
        (and task
             (funcall (plist-get task :func)))
      (error (message "Search error: %s\nSearch task: %s"
                      (error-message-string err)
                      task)))
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
  (and (timerp search-timer)
       (cancel-timer search-timer))
  (setq search-timer (run-with-idle-timer
                      search-timer-delay nil
                      'search-doer)))

(defun search-start-dequeue ()
  "Start to evaluate search task in the queue."
  (unless (timerp search-timer)
    ;; Setup timer
    (search-setup-doer)
    ;; Start prmopt.
    (search-start-prompt)))

(defun search-start-prompt ()
  )

(defun search-stop-prompt ()
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Task API for Backends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun search:chain (&rest tasks)
  )

(defun search:lambda (func)
  "Create a search object wrapping BODY. A search object is actually a LAMBDA
object."
  (search-create-task
   ;; Function.
   func
   ;; Synchronous.
   nil))

(defun search:process-shell (command)
  "Create a search object wrapping `start-process-shell-command' with COMMAND.
The output will be dumpped to a temporary buffer which will be deleted when done."
  (search-create-task
   ;; Function.
   (eval
    `(lambda (&rest args)
       (let ((proc (start-process-shell-command
                    "*search-proc*" (search-buffer) ,command)))
         (set-process-sentinel
          proc
          (lambda (proc event)
            (with-current-buffer (search-buffer)
              (save-buffer)
              (search-setup-doer)))))))
   ;; Asynchronous.
   t))

(defun search:process-shell-buffer (command)
  "Create a search object wrapping `start-process-shell-command' with COMMAND.
The output will be dumpped directly to the `search-buffer'."
  (search-create-task
   ;; Function.
   (eval
    `(lambda (&rest args)
       (let ((proc (start-process-shell-command
                    "*search-proc*" (search-buffer) ,command)))
         (set-process-sentinel
          proc
          (lambda (proc event)
            (with-current-buffer (search-buffer)
              (save-buffer)
              (search-setup-doer)))))))
   ;; Asynchronous.
   t))

;; Test >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(defun test ()
  (search-append-task
   (search:process-shell-buffer "ls -al"))
  
  (search-append-task
   (search:process-shell-buffer "ls -al /bin"))
  
  (search-append-task
   (search:lambda
    (lambda ()
      (with-current-buffer (search-buffer)
        (insert "\n123123\n")
        (save-buffer)))))

  (search-append-task
   (search:process-shell-buffer "ls -al"))

  (search-append-task
   (search:process-shell-buffer "ls -al"))

  (search-append-task
   (search:process-shell-buffer "find /Users/boyw165/.emacs.d/elpa -name \"*.el\"|xargs grep -nH def 2>/dev/null"))
  
  (search-start-dequeue))
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
  (if (string= buffer-file-name (expand-file-name search-result))
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
