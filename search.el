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

(defcustom search-exec "grep"
  "Search tool name. Default is GREP."
  :type '(choice (const :tag "grep" "grep")
                 (const :tag "ack" "ack")
                 (const :tag "ag" "ag")
                 (string :tag "User defined"))
  :group 'search)

(defcustom search-cached-file "~/.emacs.d/.search"
  "File path of cached search result."
  :type 'string
  :group 'search)

(defcustom search-tasks-max 5
  "Maximum length of the task queue."
  :type 'integer
  :group 'search)

(defconst search-params '(("grep" . "")
                          ("ack" . "")
                          ("ag" . ""))
  )

(defvar search-tasks nil
  "Task queue.")

(defun search-exec? ()
  (with-temp-buffer
    (= 0 (call-process-shell-command
          (concat "which " search-exec)
          nil (list (current-buffer) nil)))))

(defun search-buffer ()
  "Get search buffer and refresh its content."
  (let ((name (file-name-nondirectory search-cached-file)))
    (with-current-buffer (get-buffer-create name)
      (erase-buffer)
      (when (file-exists-p search-cached-file)
        (insert-file-contents-literally search-cached-file))
      (set-buffer-modified-p nil))))

(defmacro search-with-buffer (&rest body)
  "Evaluate BODY in the search result buffer."
  (declare (indent 0) (debug t))
  `(with-current-buffer (search-buffer)
     (prog1
         (progn ,@body)
       (when (buffer-modified-p)
         (save-buffer)))))

;;;###autoload
(defun search-start-search (&optional regexp &rest args)
  "FILES format:
  (:dirs (A B C ...)
   :files (1 2 3 ...)
   :input FILE)"
  (interactive
   (let* ((ans (ido-completing-read "Search current file or directory? "
                                    '("file" "directory") nil t))
          (args (cond
                 ((equal ans "file")
                  (list :files (buffer-file-name)))
                 ((equal ans "directory")
                  (list :dirs (file-name-directory (buffer-file-name)))))))
     (push (read-from-minibuffer "Search: ") args)))
  (if (< (length deferred:queue) search-tasks-max)
      (deferred:$
        (deferred:process)
        (deferred:nextc it
          (lambda (x)
            (ido-completing-read "Search is done, show or not? "
                                 '("yes" "not") nil t)
            )))
    (message "Search queue is full (max %s), please wait." search-tasks-max)))
;; deferred:queue
;; (deferred:process)

;;;###autoload
(defun search-toggle-search-result ()
  (interactive)
  (if (string= buffer-file-name search-cached-file)
      ;; TODO: Kill buffer without asking.
      (kill-buffer)
    (set-window-buffer (selected-window) (search-buffer))))

(provide 'search)
;;; search.el ends here
