;;; accumulate-text.el --- Accumulate text from various sources
;;; Originally motivated by talking to LLMs via browser-based chat

(require 'subr-x)

;; Useful LLM prompting utils - put context about current file / dir structure in system clipboard
(defun copy-file-path ()
  "Copy the current buffer's file path to the clipboard."
  (interactive)
  (when buffer-file-name
    (let* ((path (buffer-file-name)))
      (kill-new path)
      (message "File path copied to clipboard.")
      path)))
(global-set-key (kbd "C-c V") 'copy-file-path)
(defun copy-file-path-and-contents ()
  "Copy the current buffer's file path and its contents to the clipboard."
  (interactive)
  (when buffer-file-name
    (let* ((path (copy-file-path))
           (file-contents (buffer-string))
           (combined (concat path "\n" file-contents)))
      (kill-new combined)
      (message "File path and contents copied to clipboard."))))
(global-set-key (kbd "C-c C") 'copy-file-path-and-contents)
(defun copy-file-name ()
  "Copy the current buffer's file name to the clipboard."
  (interactive)
  (when buffer-file-name
    (let* ((name (file-name-nondirectory buffer-file-name)))
      (kill-new name)
      (message "File name copied to clipboard.")
      name)))
(global-set-key (kbd "C-c N") 'copy-file-name)
(defun copy-all-file-paths-and-contents ()
  "Copy the file paths and contents of all files in the current dired directory and its subdirectories to the clipboard."
  (interactive)
  (let* ((ignore-patterns '("node_modules" "\\package-lock.json" "\\.git" "dist" "outputs" "build" "\\.vscode" "\\.idea" "\\.DS_Store" "\\.log"
                           "\\.cache" "\\.tmp" "venv" "\\.next" "\\.npm" "coverage" "bower_components" "\\.lock$"
                           "\\.swp$" "\\.tmp$" "\\.gz$" "\\.zip$" "\\.tar$" "\\.rar$" "\\.jpg$" "\\.jpeg$" "\\.png$"
                           "\\.gif$" "\\.bmp$" "\\vendor" "\\pgdata" "\\public$" "\\vendor" "\\.env")) ; the list of patterns to ignore
         (all-paths-and-contents '()))
    (dolist (file (directory-files-recursively default-directory ".*"))
      (when (and (not (file-directory-p file)) ; skip directories
                 (not (seq-some (lambda (pattern) ; check if file matches any ignore pattern
                                  (string-match-p pattern file))
                                ignore-patterns)))
        (find-file file)
        (setq all-paths-and-contents
              (append all-paths-and-contents
                      (list (concat file "\n" (buffer-string)))))
        (kill-buffer)))
    (kill-new (mapconcat 'identity all-paths-and-contents "\n\n")) ; join all file paths and contents
    (message "All file paths and contents copied to clipboard.")))
(global-set-key (kbd "C-c T") 'copy-all-file-paths-and-contents)
(defun copy-current-line-to-clipboard ()
  "Copy the current line to the system clipboard without newlines."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (kill-ring-save begin end)
    (with-temp-buffer
      (yank)
      (let ((copy (substring-no-properties (buffer-string) 0 -1))) ; remove newline
        (kill-new copy)
        (message "Line copied to clipboard: %s" copy)))))
(global-set-key (kbd "C-c L") 'copy-current-line-to-clipboard)

(defun ensure-accumulation-buffer ()
  "Ensure the 'accumulated-for-llm' buffer is created and switched to."
  (let ((buf (get-buffer-create "accumulated-for-llm")))
    (with-current-buffer buf
      (goto-char (point-max)))))

(defun rk/count-lines-in-string (string)
  "Return the number of newline-delimited lines in STRING.
Counts the trailing line even if STRING lacks a terminating newline."
  (let ((len (length string))
        (idx 0)
        (lines 0))
    (while (< idx len)
      (when (= (aref string idx) ?\n)
        (setq lines (1+ lines)))
      (setq idx (1+ idx)))
    (if (and (> len 0)
             (/= (aref string (1- len)) ?\n))
        (1+ lines)
      lines)))

(defun accumulate-file-path-and-contents ()
  "Append file path and contents to the accumulation buffer."
  (interactive)
  (ensure-accumulation-buffer)
  (when buffer-file-name
    (let* ((path (buffer-file-name))
           (contents (buffer-string))
           (line-count (rk/count-lines-in-string contents))
           (data (concat "\n" path "\n" contents)))
      (with-current-buffer "accumulated-for-llm"
        (insert data)
        (message "File path and contents accumulated (%d line%s)."
                 line-count (if (= line-count 1) "" "s")))
      line-count)))
(defun accumulate-file-name ()
  "Append file name to the accumulation buffer."
  (interactive)
  (ensure-accumulation-buffer)
  (when buffer-file-name
    (let ((name (file-name-nondirectory buffer-file-name)))
      (with-current-buffer "accumulated-for-llm"
        (insert (concat "\n" name))
        (message "File name accumulated.")))))
(defun accumulate-file-path ()
  "Append file path to the accumulation buffer."
  (interactive)
  (ensure-accumulation-buffer)
  (when buffer-file-name
    (let ((path (buffer-file-name)))
      (with-current-buffer "accumulated-for-llm"
        (insert (concat "\n" path))
        (message "File path accumulated.")))))
(defun accumulated-selected-text ()
  "Append selected text to the accumulation buffer."
  (interactive)
  (ensure-accumulation-buffer)
  (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
    (with-current-buffer "accumulated-for-llm"
      (insert (concat "\n" text))
      (message "Selected text accumulated.")))
  (deactivate-mark)
  (setq deactivate-mark nil))
(defun copy-accumulated-text-to-clipboard ()
  "Copy the contents of 'accumulated-for-llm' to the clipboard."
  (interactive)
  (with-current-buffer "accumulated-for-llm"
    (clipboard-kill-ring-save (point-min) (point-max))
    (message "Accumulated data copied to clipboard.")))
(defun drop-accumulated-buffer ()
  "Clear all contents from the 'accumulated-for-llm' buffer."
  (interactive)
  (let ((buf (get-buffer "accumulated-for-llm")))
    (when buf
      (with-current-buffer buf
        (erase-buffer)  ; This clears the buffer.
        (message "Accumulated buffer cleared.")))))

;; Region-driven accumulation -------------------------------------------------
(defun rk/--extract-absolute-path (line)
  "Return the absolute file path found in LINE, or nil if none.
Common prompt markers like `> ' or bullet prefixes are ignored. The first
substring beginning with `/` is assumed to be the path."
  (let* ((trimmed (string-trim line))
         (prefixes '("> " "- " "• ")))
    ;; Remove repeated "> " prefixes the LLM output often includes.
    (while (string-prefix-p "> " trimmed)
      (setq trimmed (string-trim (substring trimmed 2))))
    ;; Drop a single bullet prefix if present.
    (dolist (prefix prefixes)
      (when (string-prefix-p prefix trimmed)
        (setq trimmed (string-trim (substring trimmed (length prefix))))))
    (let ((slash (string-match "/" trimmed)))
      (when slash
        (let* ((candidate (string-trim-right (substring trimmed slash)))
               (path (if (string-prefix-p "~/" candidate)
                         (expand-file-name candidate)
                       candidate)))
          (when (file-name-absolute-p path)
            path))))))

(defun rk/accumulate-paths-from-region (start end &optional clear-first)
  "Accumulate file contents for each absolute path between START and END.
Each line may include prompt symbols; the first `/`-prefixed segment is treated
as the path. With prefix argument CLEAR-FIRST, empty the accumulation buffer
before processing."
  (interactive "r\nP")
  (unless (use-region-p)
    (user-error "Activate a region containing absolute paths"))
  (when clear-first
    (drop-accumulated-buffer))
  (let ((lines (split-string (buffer-substring-no-properties start end) "\n" t))
        (processed 0)
        (skipped 0)
        (line-total 0))
    (dolist (line lines)
      (let ((path (rk/--extract-absolute-path line)))
        (cond
         ((not path)
          (setq skipped (1+ skipped)))
         ((not (file-exists-p path))
          (setq skipped (1+ skipped))
          (message "Skipping missing path: %s" path))
         (t
          (let* ((existing (get-file-buffer path))
                 (buffer (or existing (find-file-noselect path))))
            (condition-case err
                (let ((added-lines (with-current-buffer buffer
                                     (accumulate-file-path-and-contents))))
                  (setq processed (1+ processed))
                  (setq line-total (+ line-total (or added-lines 0))))
              (error
               (setq skipped (1+ skipped))
               (message "Error accumulating %s: %s" path err)))
            (unless existing
              (kill-buffer buffer)))))))
    (message "Accumulated %d file(s); appended %d line(s); skipped %d line(s)."
             processed line-total skipped)))

;; Dired specific accumulation
(defun dired-accumulate-file-and-next ()
  "In Dired, open file at point, accumulate its path and contents,
kill the file's buffer, return to Dired, and move to the next line.
This function is intended to be bound to C-c C in dired-mode."
  (interactive)
  (if (not (eq major-mode 'dired-mode))
      (message "This command must be run from a Dired buffer.")
    (let* ((dired-buffer (current-buffer))
           ;; dired-get-filename:
           ;; First argument (FILENAME-ONLY-P): nil means full path, not just name.
           ;; Second argument (NOERROR): t means return nil instead of error if no file at point.
           (file-to-process (dired-get-filename nil t))
           (original-window-config (and (window-live-p (selected-window))
                                        (current-window-configuration))))
      (if (and file-to-process (file-regular-p file-to-process))
          (progn
            ;; Open the file, making its buffer current.
            ;; This is necessary because `accumulate-file-path-and-contents`
            ;; operates on the `current-buffer`.
            (find-file file-to-process)
            (unwind-protect
                 ;; Main action: accumulate from the now-current file buffer.
                 ;; `accumulate-file-path-and-contents` will show its own message.
                 (accumulate-file-path-and-contents)
              ;; Cleanup: kill the file's buffer that we just processed.
              ;; (current-buffer) is the file's buffer here.
              (kill-buffer (current-buffer)))

            ;; Restore Dired window configuration and buffer focus
            (when original-window-config
              (set-window-configuration original-window-config))

            ;; Ensure Dired buffer is current for dired-next-line
            ;; (set-window-configuration should handle this if dired-buffer was
            ;; active in the selected window of original-window-config,
            ;; but an explicit switch is safer).
            (if (not (eq (current-buffer) dired-buffer))
                (switch-to-buffer dired-buffer))

            ;; Perform Dired action in the Dired buffer
            (dired-next-line 1)
            (message "Accumulated: %s and moved to next line." (file-name-nondirectory file-to-process)))
        (message "Not a regular file at point (or point is not on a file name).")))))


;; Keybindings
(global-set-key (kbd "C-c a c") 'accumulate-file-path-and-contents)
(global-set-key (kbd "C-c a n") 'accumulate-file-name)
(global-set-key (kbd "C-c a v") 'accumulate-file-path)
(global-set-key (kbd "C-c a w") 'accumulated-selected-text)
(global-set-key (kbd "C-c a a") 'copy-accumulated-text-to-clipboard)
(global-set-key (kbd "C-c a D") 'drop-accumulated-buffer)
(global-set-key (kbd "C-c a r") 'rk/accumulate-paths-from-region)
;; C-c C in dired mode will now run dired-accumulate-file-and-next
(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "C-c a C") #'dired-accumulate-file-and-next))
