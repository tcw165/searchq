;;; searchq.el --- Framework of queued search tasks using GREP, ACK, AG and more.
;;
;; Copyright (C) 2014-2015
;;
;; Author: boyw165
;; Version: 20150224.1800
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
;; A search framework which let you search string or regular expression
;; in the background and queue the search tasks. It uses `find`, `grep`,
;; `ack` or `ag` command as its searching backends. In addition, the
;; search result can be a jotting. So it keeps the search result for
;; you and provides editing function.
;;
;; TODO
;; ----
;; * Support AG.
;; * Improve interaction of `searchq-search' and `searchq-search-command'.
;; * Cancel individual search task.
;; * Open with searchq-result will cause hl-highlight-mode work incorrectly.
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

(defgroup searchq nil
  "Framework of queued search tasks using GREP, ACK, AG and more.")

(defconst searchq-default-backends
  '(("FIND and GREP" ("find" "grep") "find $pwd|xargs grep -nH -e \"$0\" 2>/dev/null"
     searchq-grep-backend)
    ("ACK only"      ("ack")         "ack --nocolor '$0' $pwd 2>/dev/null"
     searchq-ack-backend)
    ("AG only"       ("ag")          "echo constructing..."
     searchq-ag-backend))
  "Default search backends. The format:
* The 1st element is description string.
* The 2nd element is a exec path list to be tested.
* The 3rd element is a command template string.
  $0 is the cursor position; $pwd is the current directory.
* The 4th element is the backend which is the doer of everything.")

(defconst searchq-buffer-name "Searchq-Result"
  "Search buffer name.")

(defconst searchq-temp-buffer-name "*Searchq-Temp*"
  "Temporary search buffer name.")

(defcustom searchq-backends (nth 0 searchq-default-backends)
  "Search backends. See `searchq-default-backends'."
  :type `(choice ,@(mapcar (lambda (c)
                             `(const :tag ,(nth 0 c)
                                     ,c))
                           searchq-default-backends)
                 (list :tag "User Defined"
                       (string :tag "Description")
                       (repeat :tag "Necessary Exec List" (string :tag "Name"))
                       (string :tag "Command Sample")
                       (function :tag "Backend Function")))
  :group 'searchq)

(defcustom searchq-ignored-paths-for-find-command `("*.git*"
                                                    "*.svn*"
                                                    "*build*")
  "Ignored paths for FIND command."
  :type '(repeat string)
  :group 'searchq)

(defcustom searchq-string-history `("\\$\\(.*\\)")
  "Your favorite search string history"
  :type '(repeat string)
  :group 'searchq)

(defcustom searchq-file-history `(,(expand-file-name "~/"))
  "Your favorite search file/directory history"
  :type '(repeat string)
  :group 'searchq)

(defcustom searchq-saved-file (expand-file-name "~/.emacs.d/.search")
  "File for cached search result."
  :type 'string
  :set (lambda (symb val)
         (when (file-writable-p val)
           (set symb val)
           (add-to-list 'auto-mode-alist
                        `(,(format "\\%s\\'" (file-name-nondirectory val))
                          . searchq-result-mode))))
  :group 'searchq)

(defcustom searchq-temp-file (expand-file-name "/var/tmp/.searchq-tmp")
  "Temporary file for search. e.g. as an input file with context of listed
files."
  :type 'string
  :group 'searchq)

(defcustom searchq-tasks-max 5
  "Maximum length of the task queue."
  :type 'integer
  :group 'searchq)

(defcustom searchq-timer-delay 0.3
  "Delay seconds for every search task. Try don't make it less than 0.3."
  :type 'integer
  :group 'searchq)

(defcustom searchq-delimiter '(">>>>>>>>>> " . "<<<<<<<<<<")
  "Delimiter of every search task. Default is markdown style."
  :type '(cons (match :tag "open delimiter")
               (match :tag "close delimiter"))
  :group 'searchq)

(defcustom searchq-start-action-function 'searchq-switch-to-search-buffer
  "Action function whenever new search starts."
  :type 'function
  :group 'searchq)

(defcustom searchq-prompt-function 'searchq-default-prompt-function
  "Prompt function."
  :type 'function
  :group 'searchq)

(defvar searchq-tasks nil
  "[internal use]
Search tasks queue. See `searchq-create-task' for struct format.")

(defvar searchq-tasks-count 0
  "[internal use]
Search tasks count.")

(defvar searchq-proc nil
  "[internal use]
Search task process.")

(defvar searchq-timer nil
  "[internal use]
A delay timer for evaluating the queued tasks.")

(defvar searchq-prompt-timer nil
  "[internal use]
A timer for showing prompt animation.")

(defun searchq-exec? ()
  "[internal use]
Test whether the necessary exe(s) are present."
  (unless (and (executable-find "sh")
               (executable-find "xargs")
               (null (memq nil
                           (mapcar 'executable-find
                                   (nth 1 searchq-backends)))))
    (error "%s or xargs is not supported on your system!"
           (nth 1 searchq-backends)))
  t)

(defun searchq-running? ()
  "[internal use]
Test whether the search is under processing."
  (> searchq-tasks-count 0))

(defmacro searchq-with-searchq-buffer (&rest body)
  "[internal use]
Evaluate BODY in the search buffer."
  (declare (indent 0) (debug t))
  `(with-current-buffer (get-buffer-create searchq-buffer-name)
     (set-auto-mode t)
     (setq buffer-file-name (expand-file-name searchq-saved-file))
     ,@body))

(defun searchq-prepare-searchq-buffer ()
  "[internal use]
Prepare search buffer."
  (unless (get-buffer searchq-buffer-name)
    (searchq-with-searchq-buffer
      (and (file-exists-p searchq-saved-file)
           (insert-file-contents-literally
            searchq-saved-file
            nil nil nil t)))))

(defun searchq-create-task (fn is-async-fn)
  "[internal use]
Create a search task with FN function and IS-ASYNC-FN boolean."
  (list :fn fn
        :is-async-fn is-async-fn))

(defun searchq-task-p (task)
  "[internal use]
Test if the TASK is a valid search task."
  (plist-get task :fn))

(defun searchq-append-task (task)
  "[internal use]
Append TASK to `searchq-tasks' and evaluate it later. See `searchq-create-task'."
  (when (searchq-task-p task)
    ;; Append task and update destruct task.
    (setq searchq-tasks (delq searchq-destructor-task searchq-tasks)
          searchq-tasks (append searchq-tasks
                                (list task searchq-destructor-task)))))

(defun searchq-list-tasks ()
  "[internal use]
List search tasks."
  )

(defun searchq-switch-to-search-buffer ()
  "[internal use]
Switch to search buffer."
  (switch-to-buffer searchq-buffer-name))

(defun searchq-doer ()
  "[internal use]
Doer decide when and what to process next."
  (let* ((task (car searchq-tasks))
         (func (plist-get task :fn)))
    (pop searchq-tasks)
    ;; Execute current task.
    (condition-case err
        (and (functionp func)
             (funcall func))
      (error (message "searchq-doer error: %s"
                      (error-message-string err))))
    ;; Find next task.
    (when searchq-tasks
      (cond
       ((null (plist-get task :is-async-fn))
        (searchq-delay-doer))))))

(defun searchq-delay-doer ()
  "[internal use]
Run `searchq-doer' after a tiny delay."
  (and (timerp searchq-timer)
       (setq searchq-timer (cancel-timer searchq-timer)))
  (setq searchq-timer (run-with-idle-timer
                       searchq-timer-delay nil
                       'searchq-doer)))

(defun searchq-start-dequeue ()
  "[internal use]
Start to evaluate search task in the queue."
  ;; Setup timer
  (searchq-delay-doer)
  ;; Start prmopt.
  (searchq-start-prompt))

(defvar searchq-prompt-animation '("-" "\\" "|" "/")
  "[internal use]
Prompt animation.")

(defun searchq-message (message &rest args)
  "[internal use]
Display MESSAGE in the minibuffer when minibuffer is inactive."
  (when (not (minibufferp (current-buffer)))
    (if args
        (apply 'message message args)
      (message "%s" message))))

(defun searchq-default-prompt-function ()
  "[internal use]
Default prompt function."
  (let ((char (car searchq-prompt-animation)))
    (searchq-message "Search ...%s" char)
    (setq searchq-prompt-animation (cdr searchq-prompt-animation)
          searchq-prompt-animation (append
                                    searchq-prompt-animation
                                    (list char)))))

(defun searchq-start-prompt ()
  "[internal use]
Start prmopt animation."
  (when (timerp searchq-prompt-timer)
    (setq searchq-prompt-timer (cancel-timer searchq-prompt-timer)))
  (setq searchq-prompt-timer
        (run-with-timer
         0 0.1
         (lambda ()
           (and (functionp searchq-prompt-function)
                (funcall searchq-prompt-function))))))

(defun searchq-stop-prompt ()
  "[internal use]
Stop prompt animation."
  (when (timerp searchq-prompt-timer)
    (setq searchq-prompt-timer (cancel-timer searchq-prompt-timer)))
  (searchq-message "Search ...done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Task API for Backends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun searchq:chain (&rest tasks)
  "[backend api]
Chain the TASKS."
  (mapc 'searchq-append-task tasks)
  (searchq-start-dequeue)
  nil)

(defmacro searchq:lambda (&rest body)
  "[backend api]
Create a search task wrapping FUNC which is a lambda function."
  ;; Sample ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; (searchq:lambda
  ;;   (message "123"))
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  (declare (doc-string 2) (indent defun) (debug t))
  `(searchq-create-task
    ;; Function.
    (lambda ()
      (condition-case err
          (progn ,@body)
        (error (message "searchq:lambda | error: %s"
                        (error-message-string err)))))
    ;; Synchronous.
    nil))

(defmacro searchq:lambda-to-searchq-buffer (&rest body)
  "[backend api]
Create a search task wrapping FUNC under `searchq-buffer'."
  (declare (doc-string 2) (indent defun) (debug t))
  `(searchq:lambda
     (searchq-with-searchq-buffer
       (condition-case err
           (save-excursion
             ,@body)
         (error (message "searchq:lambda-to-searchq-buffer | error: %s"
                         (error-message-string err)))))))

(defun searchq:process-shell (command bufname &optional callback)
  "[backend api]
Create a search task wrapping `start-process-shell-command' with COMMAND.
The output will be dumpped to a BUFNAME buffer which will be deleted when done.
The CALLBACK is evaluated under process's buffer.
See `searchq:process-shell-to-file' or `searchq:process-shell-to-searchq-buffer'
for example."
  (searchq-create-task
   ;; Function.
   (eval
    `(lambda (&rest args)
       (let* ((buf ,(if (string= bufname searchq-buffer-name)
                        (searchq-with-searchq-buffer
                          (current-buffer))
                      (get-buffer-create (or bufname
                                             searchq-temp-buffer-name)))))
         (and (process-live-p searchq-proc)
              (setq searchq-proc (delete-process searchq-proc)))
         (setq searchq-proc (start-process-shell-command
                             "*searchq-proc*" buf ,command))
         (set-process-sentinel
          searchq-proc
          (lambda (proc event)
            (with-current-buffer (process-buffer proc)
              (condition-case err
                  ,(and (functionp callback)
                        `(funcall ,callback))
                (error (message "searchq:process-shell | error: %s"
                                (error-message-string err))))
              ;; Kill buffer if it is a temporary buffer.
              (and (string= searchq-temp-buffer-name
                            (buffer-name))
                   (kill-buffer)))
            (searchq-delay-doer))))))
   ;; Asynchronous.
   t))

(defun searchq:process-shell-to-file (command filename)
  "[backend api]
Create a search task wrapping `start-process-shell-command' with COMMAND.
The output will be written to FILENAME file."
  (searchq:process-shell
   command (file-name-nondirectory filename)
   (eval `(lambda ()
            ;; Prepend the content if file is alreay existed.
            (when (file-exists-p ,filename)
              (goto-char (point-min))
              (insert-file-contents-literally))
            (setq buffer-file-name ,filename)
            (save-buffer)
            (kill-buffer)))))

(defun searchq:process-shell-to-searchq-buffer (command)
  "[backend api]
Create a search task wrapping `start-process-shell-command' with COMMAND.
The output will be dumpped directly to the `searchq-buffer'."
  (searchq:process-shell
   command searchq-buffer-name
   (lambda ()
     (setq buffer-file-name (expand-file-name searchq-saved-file)))))

(defvar searchq-clean-temp-file-task
  (searchq:lambda
    (and (file-exists-p searchq-temp-file)
         (delete-file searchq-temp-file)))
  "[backend api]
Search task to clean temporary file.")

(defvar searchq-print-closed-delimiter
  (searchq:lambda-to-searchq-buffer
    (setq searchq-tasks-count (1- searchq-tasks-count))
    (goto-char (point-max))
    (unless (looking-back "[\r\n]")
      (insert "\n"))
    (insert (cdr searchq-delimiter) "\n\n")
    (save-buffer))
  "[backend api]
Search task to print closed delimiter.")

;; !important! The last search task.
(defvar searchq-destructor-task
  (searchq:lambda
    ;; Stop prompt.
    (searchq-stop-prompt)
    ;; Reset counter.
    (setq searchq-tasks-count 0))
  "[internal use]
Destructor-liked search task which is always the last one in the queue.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun searchq-dummy-backend (&rest args)
  "[internal use]
Just a dummy backend. See `searchq-search'.")

(defun searchq-gen-find-filter (include exclude)
  "[internal use]
Take INCLUDE and EXCLUDE arguments and generate FIND command string. The format
depends on the FIND's option, --path."
  ;; Sample ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; (searchq-gen-find-filter nil nil)
  ;; (searchq-gen-find-filter '("*.el" "*.txt") nil)
  ;; (searchq-gen-find-filter '("*.el" "*.txt") '(".git" ".svn"))
  ;; (searchq-gen-find-filter nil '(".git" ".svn"))
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  (ignore-errors
    (let (icmd xcmd)
      (setq exclude (append exclude searchq-ignored-paths-for-find-command))

      (when include
        (mapc (lambda (exp)
                (setq icmd (concat icmd " -path \"" exp "\"")))
              include))
      (when exclude
        (mapc (lambda (exp)
                (setq xcmd (concat xcmd " -not -path \"" exp "\"")))
              exclude))
      (concat icmd xcmd))))

(defun searchq-grep-backend (args)
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
     `(searchq:chain
       ;; Prepare input file.
       ;; FILES part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ,(when files
          `(searchq:lambda
             (with-temp-file searchq-temp-file
               (mapc (lambda (path)
                       (and (file-exists-p path)
                            (insert (expand-file-name path) "\n")))
                     ,files))))
       ;; DIRS part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ,@(mapcar (lambda (path)
                   `(searchq:process-shell-to-file
                     ,(format "find %s %s 2>/dev/null"
                              (expand-file-name path)
                              (searchq-gen-find-filter include exclude))
                     searchq-temp-file))
                 real-dirs)
       ;; FROMFILE part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ,(when fromfile
          `(searchq:lambda
             (with-current-buffer (find-file-noselect searchq-temp-file)
               (insert "\n")
               (insert-file-contents-literally ,fromfile))))
       ;; Start to search by using input file.
       (searchq:process-shell-to-searchq-buffer
        ,(format "xargs grep -nHE '%s' <'%s' 2>/dev/null"
                 match
                 (expand-file-name searchq-temp-file)))))))

(defun searchq-gen-ack-filter (include exclude)
  "[internal use]
Take INCLUDE and EXCLUDE arguments and generate ACK command string. The format
depends on ACK's option,
--type-set=include:ext:??? for includes;
--type-set=exclude:ext:??? for excludes."
  ;; Sample ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; (searchq-gen-ack-filter nil nil)
  ;; (searchq-gen-ack-filter '("el" "txt" "md") nil)
  ;; (searchq-gen-ack-filter nil '("git" "svn"))
  ;; (searchq-gen-ack-filter '("el" "txt") '("git" "svn"))
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

(defun searchq-ack-backend (args)
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
     `(searchq:chain
       ;; Prepare input file.
       ;; FILES part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ,(when files
          `(searchq:lambda
             (with-temp-file searchq-temp-file
               (mapc (lambda (path)
                       (insert path "\n"))
                     ,files))))
       ;; DIRS part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; ,(when dirs
       ;;    `(searchq:lambda
       ;;       (with-current-buffer (find-file-noselect searchq-temp-file)
       ;;         (mapc (lambda (path)
       ;;                 (and (file-exists-p path)
       ;;                      (insert (expand-file-name path) "\n")))
       ;;               ,dirs)))
       ;;    `(searchq:process-shell-to-file
       ;;      ,(format "xargs ack -f %s <%s"
       ;;               (searchq-gen-ack-filter include exclude)
       ;;               (expand-file-name searchq-temp-file))
       ;;      searchq-temp-file))
       ;; TODO: improve speed
       ,@(mapcar (lambda (path)
                   `(searchq:process-shell-to-file
                     ,(format "ack -f %s %s"
                              (searchq-gen-ack-filter include exclude)
                              (expand-file-name path))
                     searchq-temp-file))
                 real-dirs)
       ;; FROMFILE part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ,(when fromfile
          `(searchq:lambda
             (with-current-buffer (find-file-noselect searchq-temp-file)
               (insert "\n")
               (insert-file-contents-literally ,fromfile))))
       ;; Start to search by using input file.
       (searchq:process-shell-to-searchq-buffer
        ,(format "xargs ack --nocolor \"%s\" <\"%s\" 2>/dev/null"
                 match
                 (expand-file-name searchq-temp-file)))))))

(defun searchq-ag-backend (args)
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
     `(searchq:chain
       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public & Interactive API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun searchq-search (match &rest args)
  "Make a search task to search MATCH string or regular expression refer to
attributes ARGS. ARGS describes what files, or what directories to search.
Search tasks is delegated to `searchq-backends'.

Format of ARGS
--------------
* Search MATCH in specific files:
  ARGS = :files '(filename1 filename2 filename3 ...)
* Search MATCH in specific directories:
  ARGS = :dirs '(INCLUDES EXCLUDES dirpath1 dirpath2 dirpath3 ...)
  INCLUDES = A filter list for including files.
             ex: '(*.md *.txt) for `searchq-grep-backend'.
  EXCLUDES = A filter list for excluding files.
             ex: '(*.git* *.svn*) for `searchq-grep-backend'.
  !Note: The format of INCLUDES and EXCLUDES depends on the backend you're using.
* Search MATCH in files listed in an input file.
  ARGS = :fromfile filename

Example
-------
* Search MATCH in specific files and directories.
  (searchq-search MATCH :files '(/path/a /path/b) :dirs '(nil nil /path/dir1 /path/dir2))
* Search MATCH in files listed in an input file.
  (searchq-search MATCH :fromfile /path/inputfile)
* Search MATCH in specific directories and ignore subversion files.
  (searchq-search MATCH :dirs '(nil (*.git* *.svn*) /path/dir1 /path/dir2))
"
  (interactive
   (let* ((match (read-from-minibuffer
                  "Searchq Regex Search: "
                  (let ((bounds (if (region-active-p)
                                    (cons (region-beginning) (region-end))
                                  (bounds-of-thing-at-point 'symbol))))
                    (and bounds
                         (regexp-quote
                          (buffer-substring-no-properties (car bounds)
                                                         (cdr bounds)))))
                  nil nil 'searchq-string-history))
          (path (expand-file-name
                 ;; TODO: read file or directory name.
                 ;; TODO: history.
                 (read-directory-name
                  (format "Search %s in: " match)
                  (car searchq-file-history)))))
     (cond
      ((file-regular-p path)
       (list match :files `(,path)))
      ((file-directory-p path)
       (list match :dirs `(nil nil ,path))))))
  (searchq-exec?)
  (when (stringp match)
    (if (< searchq-tasks-count searchq-tasks-max)
        (eval
         `(progn
            ;; Increase counter.
            (setq searchq-tasks-count (1+ searchq-tasks-count))
            (searchq-prepare-searchq-buffer)
            (funcall searchq-start-action-function)
            ;; Start searching.
            (searchq:chain
             searchq-clean-temp-file-task
             ;; Print opened delimiter.
             (searchq:lambda-to-searchq-buffer
               (goto-char (point-max))
               (insert (car searchq-delimiter) ,match "\n")))
            ;; Delegate to `searchq-backends' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            (funcall (nth 3 searchq-backends)
                     ',(append (list :match match) args))
            ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            (searchq:chain
             searchq-clean-temp-file-task
             searchq-print-closed-delimiter)))
      (message
       "Search string, \"%s\", is denied due to full queue."
       match))
    ;; Return tasks count.
    searchq-tasks-count))

;;;###autoload
(defun searchq-search-command (cmd)
  "Very similar to `searchq-search' but it takes CMD arguement and pass it to
`searchq-backends' directly."
  (interactive
   (let* ((prompt "Search Command: ")
          (cmd (replace-regexp-in-string
                "\\$pwd" (concat "\"" default-directory "\"")
                (nth 2 searchq-backends)))
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
                                   (nth 2 searchq-backends)))
                      (regexp (replace-regexp-in-string
                               "\\\\\\$0" "\\\\(.*\\\\)"
                               (regexp-quote cmd-sample))))
                 (and (string-match regexp cmd)
                      (match-string 1 cmd)))))
    (searchq-exec?)
    (searchq-prepare-searchq-buffer)
    (funcall searchq-start-action-function)
    ;; Start searching.
    (searchq:chain
     ;; Print opened delimiter.
     (eval
      `(searchq:lambda-to-searchq-buffer
         (goto-char (point-max))
         (insert (car searchq-delimiter) ,(or thing cmd) "\n")))
     ;; Delegate to `searchq-backends' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     (searchq:process-shell-to-searchq-buffer cmd)
     ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     searchq-print-closed-delimiter)))

;;;###autoload
(defun searchq-toggle-result ()
  (interactive)
  (if (string= (buffer-name) searchq-buffer-name)
      ;; Hide search buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (progn
        (and (buffer-modified-p)
             (save-buffer))
        (switch-to-prev-buffer))
    ;; Show search buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (if (get-buffer searchq-buffer-name)
        (switch-to-buffer searchq-buffer-name)
      ;; `searchq-result-mode' is applied automatically.
      (find-file searchq-saved-file))))

;;;###autoload
(defun searchq-stop (index)
  "Stop search task of INDEX index."
  (interactive '(0))
  (if (featurep 'helm)
      (progn
        (message "feature constructing..."))
    (message "helm package is necessary but you don't have it installed.")))

;;;###autoload
(defun searchq-stop-all ()
  "Stop all search tasks."
  (interactive)
  ;; Kill asynchronous tasks and let synchronous tasks continue.
  (dolist (task searchq-tasks)
    (and (plist-get task :is-async-fn)
         (setq searchq-tasks (delq task searchq-tasks))))
  ;; Stop async process.
  (when (process-live-p searchq-proc)
    (setq searchq-proc (delete-process searchq-proc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major Mode for Search Result ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup searchq-result nil
  "Project result mode for .result file."
  :group 'searchq)

(defcustom searchq-result-mode-hook `(save-place-find-file-hook
                                      font-lock-mode
                                      linum-mode
                                      hl-line-mode
                                      ,(and (featurep 'whereis)
                                            'whereis-symbol-mode))
  "Hook run when entering `searchq-result-mode' mode."
  :type 'hook
  :group 'searchq-result)

(defvar searchq-result-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    ;; (define-key map [up] )
    ;; (define-key map [down] )
    (define-key map [return] 'searchq-result-find-file)
    (define-key map [?d] 'searchq-result-delete-item-atpt)
    (define-key map [?D] 'searchq-result-delete-item-block)
    (define-key map [?q] 'searchq-toggle-result)
    (define-key map [?s] 'searchq-stop)
    (define-key map [?S] 'searchq-stop-all)
    (define-key map [escape] 'searchq-toggle-result)
    map)
  "[internal use]
Keymap for `searchq-result-mode'.")

(defvar searchq-result-mode-font-lock-keywords
  `((;; Delimiter and match string.
     (,(format "^%s\\(.*\\)" (regexp-quote (car searchq-delimiter))) (1 'searchq-highlight-face))
     ;; GREP style.
     ("^\\([[:alnum:] $_\/.+-]+\\):\\([0-9]+\\):" (1 'searchq-file-face) (2 'searchq-linum-face))
     ;; ACK style.
     ("^\\([a-Z]:\\\\\\|~/\\|/\\).*$" . 'searchq-file-face)
     ("^\\([0-9]+\\):" (1 'searchq-linum-face))
     ;; TODO: AG style.
     )
    ;; don't use syntactic fontification.
    t
    ;; Case insensitive.
    nil)
  "[internal use]
Font lock keywords for `searchq-result-mode'. See `font-lock-defaults' and
`font-lock-keywords'.")

(defun searchq-imenu-create-index ()
  "[internal use]
Return imenu index for `searchq-result-mode'. See `imenu--index-alist' for the
format of the buffer index alist."
  ;; (when (and (string= (buffer-name) searchq-buffer-name)
  ;;            (not (searchq-running?)))
  (when (string= (buffer-name) searchq-buffer-name)
    (let (index)
      (save-excursion
        (goto-char (point-max))
        (while (re-search-backward
                (concat "^" (regexp-quote (car searchq-delimiter)) "\\(.*\\)$")
                nil t)
          (push (cons (match-string-no-properties 1)
                      (line-end-position)) index))
        (list (cons "Search Task" index))))))

(defun searchq-result-is-valid-item ()
  "[internal use]
Test valid item at point."
  (save-excursion
    (beginning-of-line)
    (not (or (looking-at (regexp-quote (car searchq-delimiter)))
             (looking-at (regexp-quote (cdr searchq-delimiter)))
             (looking-at "$")))))

(defun searchq-result-clean-empty-item (beg end)
  "[internal use]
Delete invalid item."
  (save-excursion
    ;; Create end-mark so that the end position will be updated automatically
    ;; by insert/delete.
    (let ((end-mark (set-marker (copy-marker (mark-marker) t) end)))
      (goto-char beg)
      (while (and
              (re-search-forward (format "%s.*[\n\r]%s"
                                         (regexp-quote (car searchq-delimiter))
                                         (regexp-quote (cdr searchq-delimiter)))
                                 nil t)
              (< (point) (marker-position end-mark)))
        (goto-char (match-beginning 0))
        (delete-region (line-beginning-position 1)
                       (line-beginning-position 4))))))

(defun searchq-result-delete-item-atpt ()
  "[internal use]
Delete item at point."
  (interactive)
  (if (region-active-p)
      ;; Create end-mark so that the end position will be updated automatically
      ;; by insert/delete.
      (let ((end-mark (set-marker (copy-marker (mark-marker) t) (region-end))))
        (goto-char (region-beginning))
        (beginning-of-line)
        (setq mark-active nil)
        (while (< (point) (marker-position end-mark))
          (if (searchq-result-is-valid-item)
              (delete-region (line-beginning-position 1)
                             (line-beginning-position 2))
            (forward-line)))
        (set-marker end-mark nil))
    (and (searchq-result-is-valid-item)
         (delete-region (line-beginning-position 1)
                        (line-beginning-position 2))))
  (searchq-result-clean-empty-item (line-beginning-position -2)
                                   (line-end-position 3))
  (and (buffer-modified-p) (save-buffer)))

(defun searchq-result-delete-item-block ()
  "[internal use]
Delete item block."
  (interactive)
  (save-excursion
    (let ((beg (re-search-backward (car searchq-delimiter) nil t))
          (end (re-search-forward (format "%s.*$" (cdr searchq-delimiter)) nil t)))
      (when (and beg end)
        (setq end (progn
                    (forward-char)
                    (point)))
        (delete-region beg end)
        (and (buffer-modified-p) (save-buffer))))))

(defun searchq-result-find-file ()
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
    (when (and (stringp file)
               (file-exists-p file))
      (find-file file)
      (goto-char 1)
      (forward-line (1- linum))
      (end-of-line)
      (recenter 3))))

;;;###autoload
(define-derived-mode searchq-result-mode nil "Searchq-Result"
  "Major mode for search buffers."
  :group 'searchq-result
  (remove-overlays)
  (setq font-lock-defaults searchq-result-mode-font-lock-keywords
        truncate-lines t)
  ;; Set local imenu generator.
  (setq-local imenu-create-index-function 'searchq-imenu-create-index)
  ;; Rename buffer to `searchq-buffer-name'
  (rename-buffer searchq-buffer-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup searchq-result-face nil
  "Additional faces for `hl-anything'."
  :group 'searchq-result)

(defface searchq-file-face
  '((t (:inherit font-lock-builtin-face :underline t :weight bold)))
  "Default face for file path. Suggest no background, which will be overridden
by `hl-line-mode' or `global-hl-line-mode'."
  :group 'searchq-result-face)

(defface searchq-linum-face
  '((t (:foreground "maroon1")))
  "Default face for linum number. Suggest no background, which will be overridden
by `hl-line-mode' or `global-hl-line-mode'."
  :group 'searchq-result-face)

(defface searchq-highlight-face
  '((t (:foreground "gold" :weight bold :height 1.3)))
  "Default face for highlighting keyword in definition window."
  :group 'searchq-result-face)

;; Add faces to `hl-highlight-special-faces'.
(when (featurep 'hl-anything)
  (add-to-list 'hl-highlight-special-faces 'searchq-highlight-face))

;; Integrate with `history' if any.
(when (featurep 'history)
  (add-to-list 'history-advised-before-functions 'searchq-result-find-file)
  (add-to-list 'history-advised-after-functions 'searchq-result-find-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Menu & Toolbar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Menu items.
;; Tool-bar buttons.
(when tool-bar-mode
  (define-key-after tool-bar-map [searchq-search]
    '(menu-item "Search Thing" searchq-search
                :image (find-image '((:type xpm :file "images/searchq-search.xpm"))))))

;; `kill-eamcs-hook'.
(add-hook 'kill-emacs-hook 'searchq-stop-all)

(provide 'searchq)
;;; searchq.el ends here
