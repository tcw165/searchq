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

(defcustom search-delimiter '(">>>>>" . "<<<<<")
  "Maximum length of the task queue."
  :type '(cons (match :tag "open delimiter")
               (match :tag "close delimiter"))
  :group 'search)

(defvar search-tasks 0
  "Search tasks counter.")

(defun search-exec? ()
  "Test whether the necessary exe(s) are present."
  (unless (and (executable-find "sh")
               (executable-find "find")
               (executable-find "xargs")
               (executable-find (car search-backends)))
    (error "%s or xargs is not supported on your system!" (car search-backends))))

(defun search-buffer ()
  "Get search buffer and refresh its content."
  (let ((name (file-name-nondirectory search-result)))
    (with-current-buffer (get-buffer-create name)
      (erase-buffer)
      ;; Insert content from cached file.
      (when (file-exists-p search-result)
        (insert-file-contents-literally search-result))
      ;; Make buffer unmodified
      (set-buffer-modified-p nil)
      ;; Set buffer file name.
      (setq buffer-file-name (expand-file-name search-result))
      (current-buffer))))

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

;; deferred:queue

;; (progn
;;   (search-string "fun" :dirs `(,(expand-file-name "~/.emacs.d/oops") ,(expand-file-name "~/_CODE/ycmd/ycmd")))
;;   (search-string "fun" :dirs `(,(expand-file-name "~/.emacs.d/oops")))
;;   (search-string "process" :files (expand-file-name "~/.emacs.d/oops/.emacs"))
;;   (search-string "search" :dirs (expand-file-name "~/.emacs.d/oops"))
;;   (search-string "qu" :dirs (expand-file-name "~/.emacs.d/oops"))
;;   (search-string "def" :dirs (expand-file-name "~/.emacs.d/oops"))
;;   (search-string "var" :dirs (expand-file-name "~/.emacs.d/oops"))
;;   (search-string "defun" :dirs (expand-file-name "~/.emacs.d/oops"))
;;   (search-string "highlight" :dirs (expand-file-name "~/.emacs.d/oops"))
;;   (search-string "anything" :dirs (expand-file-name "~/.emacs.d/oops")))

(defun search-ack-backend (match &optional files dirs fromfile filters)
  "Return a deferred object which doing search job with ACK. See `search-thing'."
  )

;; (defun search-ag-backend (match &optional filters)
;;   )

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
                        (deferred:wait 50)
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
