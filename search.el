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

;; 3rd party libary.
(require 'deferred)

(defgroup search nil
  "Search")

(defcustom search-exec-path "grep"
  "Path of search tool. Default is GREP."
  :type 'string
  :group 'search)

(defcustom search-tasks-max 5
  "Maximum length of the task queue."
  :type 'integer
  :group 'search)

(defvar search-tasks nil
  "Task queue.")

(defun search-exec? ()
  (with-temp-buffer
    (= 0 (call-process-shell-command
          (concat "which " search-exec-path)
          nil (list (current-buffer) nil)))))

;;;###autoload
(defun search-start-search (&optional regexp files)
  "FILES format:
  (:dir (A B C ...)
   :files (1 2 3 ...)
   :input FILE)
"
  (interactive)
  )
;; deferred:queue

;;;###autoload
(defun search-toggle-search-result ()
  (interactive)
  )

(provide 'search)
;;; search.el ends here
