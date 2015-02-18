;; Copyright (C) 2014
;;
;; Author: BoyW165
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2015-02-08 (0.0.1)
;;    Initial release.

;; GNU library.
(require 'font-lock)
(require 'hl-line)
(require 'imenu)
(require 'saveplace)

;; 3rd party library.
(require 'hl-anything)

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
    (define-key map [return] 'search-result-open-item)
    (define-key map [?q] 'search-toggle-search-result)
    (define-key map [escape] 'search-toggle-search-result)
    (define-key map [?d] 'search-result-kill-item-at-point)
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
        (list (cons "Search Item" index))))))

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

(defun search-result-kill-item-at-point ()
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

(defun search-result-open-item ()
  "[internal use]
Open search item."
  (interactive)
  (beginning-of-line)
  (when (looking-at "^\\(.+\\):\\([0-9]+\\):")
    (let ((file (match-string 1))
          (linum (string-to-int (match-string 2))))
      (message "ready to open:%s" file)
      (when (file-exists-p file)
        (find-file file)
        (goto-char 1)
        (forward-line (1- linum))
        (end-of-line)
        (recenter 3)))))

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

(provide 'search-result-mode)
