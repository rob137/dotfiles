;;; accumulate-text.el --- Accumulate text from various sources
;;; Originally motivated by talking to LLMs via browser-based chat

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
(defun accumulate-file-path-and-contents ()
  "Append file path and contents to the accumulation buffer."
  (interactive)
  (ensure-accumulation-buffer)
  (when buffer-file-name
    (let* ((path (buffer-file-name))
           (contents (buffer-string))
           (data (concat "\n" path "\n" contents)))
      (with-current-buffer "accumulated-for-llm"
        (insert data)
        (message "File path and contents accumulated.")))))
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

;; Keybindings
(global-set-key (kbd "C-c a c") 'accumulate-file-path-and-contents)
(global-set-key (kbd "C-c a n") 'accumulate-file-name)
(global-set-key (kbd "C-c a v") 'accumulate-file-path)
(global-set-key (kbd "C-c a w") 'accumulated-selected-text)
(global-set-key (kbd "C-c a a") 'copy-accumulated-text-to-clipboard)
(global-set-key (kbd "C-c a D") 'drop-accumulated-buffer)
