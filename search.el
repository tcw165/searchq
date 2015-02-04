;;; hl-anything.el --- Highlight symbols, selections, enclosing parens and more.
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 
;;
;;; Code:

;; GNU library.
(require 'ido)

;; 3rd party libary.
(require 'deferred)

(defgroup search nil
  "Search")

(defconst search-default-commands '(("grep" . search-grep-cmd-gen)
                                    ("ack" . search-ack-cmd-gen)
                                    ;; ("ag" . search-ag-cmd-gen)
                                    )
  "Default alist of searching commands.")

(defcustom search-command (nth 0 search-default-commands)
  "Search tool name. Default is GREP."
  :type `(choice ,@(mapcar (lambda (c)
                             `(const :tag ,(car c) ,(car c) ,(cdr c)))
                           search-default-commands)
                 (cons :tag "user defined"
                       (match :tag "exec name"
                               "dummy")
                       (function :tag "command generator function"
                                 search-dummy-cmd-gen)))
  :group 'search)

(defcustom search-print-function 'search-default-print
  "Function of printing search result."
  :type 'function
  :group 'search)

(defcustom search-result (expand-file-name "~/.emacs.d/.search")
  "File for cached search result."
  :type 'match
  :group 'search)

(defcustom search-input-file "/var/tmp/.search-input-file"
  "File for input files list of search tool."
  :type 'match
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
  "Task counter.")

(defun search-exec? ()
  (unless (and (executable-find "sh")
               (executable-find "find")
               (executable-find "xargs")
               (executable-find (car search-command)))
    (error "%s or xargs is not supported on your system!" (car search-command))))

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

(defmacro search-with-inputfile (filename &rest body)
  "Export `data' to `filename' file.."
  (declare (indent 1) (debug t))
  `(when (file-writable-p ,filename)
     (with-temp-file ,filename
       (and (file-exists-p ,filename)
            (insert-file-contents-literally ,filename))
       (progn ,@body))))

(defun search-default-print (inputfile match result)
  (setq search-tasks (1- search-tasks))
  ;; Delete inputfile.
  (and (file-exists-p inputfile)
       (delete-file inputfile))
  ;; Print.
  (with-current-buffer (search-buffer)
    (goto-char (point-max))
    (insert "\n"
            (car search-delimiter) " " match "\n"
            result
            (cdr search-delimiter) "\n")
    (save-buffer))
  (message "deferred:queue=%s" (length deferred:queue))
  ;; (message "Search done, call `search-toggle-search-result' to open it.")
  )

(defun search-seralize-list (thing)
  (cond
   ;; A list of strings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ((listp thing)
    (let ((it thing)
          (space "")
          str)
      (while it
        (ignore-errors
          (setq str (concat str
                            space
                            (car it))))
        (setq it (cdr it)
              space " "))
      str))
   ;; A match ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ((stringp thing)
    thing)))

(defun search-dummy-cmd-gen (inputfile match)
  "See `search-thing'."
  )

(defun search-grep-cmd-gen (inputfile match)
  "See `search-thing'."
  (format "grep -nH --file=%s %s 2>/dev/null" inputfile match))
;; deferred:queue
;; (progn
;;   (search-string "process" :dirs (expand-file-name "~/.emacs.d/oops"))
;;   (search-string "process" :files (expand-file-name "~/.emacs.d/oops/.emacs"))
;;   (search-string "search" :dirs (expand-file-name "~/.emacs.d/oops"))
;;   (search-string "qu" :dirs (expand-file-name "~/.emacs.d/oops"))
;;   (search-string "def" :dirs (expand-file-name "~/.emacs.d/oops"))
;;   (search-string "var" :dirs (expand-file-name "~/.emacs.d/oops"))
;;   (search-string "defun" :dirs (expand-file-name "~/.emacs.d/oops"))
;;   (search-string "highlight" :dirs (expand-file-name "~/.emacs.d/oops"))
;;   (search-string "anything" :dirs (expand-file-name "~/.emacs.d/oops")))

(defun search-ack-cmd-gen (inputfile match filters)
  "See `search-thing'."
  )

;; (defun search-ag-cmd-gen (match &optional filters)
;;   )

;;;###autoload
(defun search-string (&optional match &rest args)
  "FILES format:
  (:files (1 2 3 ...)
   :dirs (A B C ...)
   :fromfile FILE
   :filters (include . exclude))"
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
  ;; (message "regexp=%s, args=%s" match args)
  (when (stringp match)
    (if (< search-tasks search-tasks-max)
        (lexical-let ((match match)
                      (files (plist-get args :files))
                      (dirs (plist-get args :dirs))
                      (fromfile (plist-get args :fromfile))
                      (filters (plist-get args :filters))
                      (inputfile (concat search-input-file "."
                                         (number-to-string (float-time)))))
          (setq search-tasks (1+ search-tasks))
          ;; TODO: filters
          (deferred:$
            ;; Prepare input file --------------------------------------------->
            ;; `:files' part.
            (deferred:new
              (lambda ()
                (when files
                  ;; Print files to INPUTFILE line by line.
                  (search-with-inputfile inputfile
                    (cond
                     ((stringp files) (insert files))
                     ((listp files) (mapc (lambda (str)
                                            (insert str "\n"))
                                          files)))))))
            ;; `:dirs' part.
            (deferred:nextc it
              (lambda (&optional x)
                (when dirs
                  (deferred:process-shell
                    ))))
            (deferred:nextc it
              (lambda (&optional x)
                (search-with-inputfile inputfile
                  )))
            ;; `:fromfile' part.
            (deferred:nextc it
              (lambda (&optional x)
                (when fromfile
                  (search-with-inputfile inputfile
                    (insert "\n")
                    (insert-file-contents-literally fromfile)))))
            ;; <----------------------------------------------------------------
            ;; Start to search.
            (deferred:nextc it
              (lambda (&optional x)
                (deferred:process-shell
                  ;; Delegate to `search-command'.
                  (apply (cdr search-command) inputfile match))))
            ;; Print the result.
            (deferred:nextc it
              (lambda (result)
                ;; Default value of `search-print-function' is `search-default-print'.
                (funcall search-print-function inputfile match result)))))
      (message "Search queue is full (max %s), please wait." search-tasks-max))))

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
