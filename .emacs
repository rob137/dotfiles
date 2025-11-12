;; ============= PACKAGE MANAGEMENT =============

;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("46c65f6d9031e2f55b919b1486952cddcc8e3ee081ade7eb2ffb6a68a804d30e"
     "b64a60e69617b4348d0402fad2f0d08a694301132e7ab243dab4d6a65c3bf948"
     "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644"
     "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294"
     "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee"
     "7e068da4ba88162324d9773ec066d93c447c76e9f4ae711ddd0c5d3863489c52"
     "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138"
     "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce"
     "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00"
     "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef"
     default))
 '(git-gutter:update-interval 2)
 '(grep-files-aliases
   '(("all" . "* .*") ("el" . "*.el") ("ch" . "*.[ch]") ("c" . "*.c")
     ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++")
     ("cchh" . "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
     ("hh" . "*.hxx *.hpp *.[Hh] *.HH *.h++") ("h" . "*.h")
     ("l" . "[Cc]hange[Ll]og*")
     ("am" . "Makefile.am GNUmakefile *.mk") ("m" . "[Mm]akefile*")
     ("tex" . "*.tex") ("texi" . "*.texi") ("asm" . "*.[sS]")
     ("py" . "*.py") ("ts" . "*.ts") ("js" . "*.js") ("css" . "*.css")
     ("html" . "*.html") ("json" . "*.json") ("jsonl" . "*.jsonl")
     ("jsonld" . "*.jsonld") ("sh" . "*.sh")))
 '(org-agenda-files '("~/notes/todo.org"))
 '(package-selected-packages
   '(auto-org-md chatgpt company-quickhelp concurrent cov coverage
                 csv-mode ctable deferred docker-compose-mode
                 dockerfile-mode doom-themes dtrt-indent ef-themes epc
                 eslint-rc evil-collection exec-path-from-shell
                 expand-region flymake-eslint flymake-ruff fzf
                 git-gutter git-modes grip-mode highlight hl-todo
                 jenkinsfile-mode json-mode magit markdown-mode
                 markdown-preview-mode prettier-js prettier-rc
                 python-mode quelpa quelpa-use-package rainbow-csv
                 restclient sass-mode scss-mode tree-sitter
                 tree-sitter-langs treesit-auto typescript-mode vterm
                 web-mode which-key))
 '(package-vc-selected-packages
   '((rainbow-csv :vc-backend Git :url
                  "https://github.com/NivWeisman/rainbow-csv")))
 '(safe-local-variable-values
   '((eval add-to-list 'grep-find-ignored-files "*.json")
     (vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (cov-lcov-file-name
      . "/Users/robert.kirby/g/aff-services-mono/apps/bookmaker-aliases-servicex/coverage/unit/lcov.info")
     (cov-lcov-file-name
      . "/Users/robert.kirby/g/aff-services-mono/apps/bookmaker-aliases-service/coverage/unit/lcov.info")
     (cov-lcov-file-name
      . "/Users/robert.kirby/g/events-service/coverage/unit/lcov.info")
     (cov-lcov-file-name
      . "/Users/robert.kirby/g/events-service/coverage/lcov.info")
     (lcov-file-name
      . "/Users/robert.kirby/g/events-service/coverage/lcov.info"))))




























;; ============= Emacs NATIVE SETTINGS =============

;; hide default ui / message stuff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq-default inhibit-startup-message t)
(setq initial-scratch-message nil)

;; dired-style buffer menu
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; dired settings
(setq
 ;; Instead of refusing..
 dired-create-destination-dirs 'ask
 ;; Update directory listings automatically
 dired-do-revert-buffer t
 )

;; Better performance with v long lines (noticeable)
(global-so-long-mode)

;; Make emacs windows transparent
(set-frame-parameter (selected-frame) 'alpha '(98 . 98))
(add-to-list 'default-frame-alist '(alpha . (98 . 98)))

;; add a 3 line buffer between the point and the top / bottom of the window
(setq scroll-margin 3)

;; scroll up with point when it reaches top / bottom of window
(setq scroll-conservatively 101) ;; 101 just means it never recenters

;; remember point in window
(setq switch-to-buffer-preserve-window-point t)

;; make version control work better, prevent warnings about opening simlinks
(setq-default vc-follow-symlinks t)

;; show number of search matches when using C-s / C-r
(setq-default isearch-lazy-count t)
(setq-default isearch-allow-scroll t)
(setq-default lazy-highlight-initial-delay 0)

 ;; navigate window layout history with C-c left / right
(winner-mode t)

(setq confirm-kill-emacs 'y-or-n-p) ;; ask before killing emacs
(setq confirm-kill-processes nil) ;; don't ask before killing processes on exit

;; Autosave 'desktop' (buffers + window layout) of current session(face tabs spaces trailing lines-tail newline empty space-before-tab space-after-tab space-mark tab-mark newline-mark)
;; desktop-save desktop-read desktop-change desktop-clear are all useful interactive functions
(desktop-save-mode 1)
(setq desktop-load-locked-desktop t) ;; Don't stop me to ask about loading a locked desktop
(setq desktop-missing-file-warning t) ;; Just warn me if a file is missing, don't offer to recreate a buffer for it

;; Save minibuffer history
(savehist-mode 1)

;; instead of icomplete - covers M-x and help too
;; http://xahlee.info/emacs/emacs/emacs_fido_mode.html
;; Removed ido-mode as it was causing issues with project grep
(fido-mode t)
;; display results in vertical list
(fido-vertical-mode t)

;; ;; show tabs, spaces, newlines etc
(global-whitespace-mode t)
;; Some of these are noisy, so I've disabled them.
;; Others are worse on certain color themes.
;; Can edit these on the fly with M-x globall-whitespace-toggle-options - follow up with ? to see options and bindings
(setq-default whitespace-style '(
                                 face ;; necessary for some of the others in this list
                                 trailing
                                 space-after-tab ;; doesn't seem to work
                                 ))

;; Thinking I'll remove, just trying life without it for a while - 11 Nov 2025
;; ;; vertical line on 80 char column
;; (setq-default display-fill-column-indicator-column 79)
;; (global-display-fill-column-indicator-mode t)

;; easier to find cursor
(global-hl-line-mode 1)

;; an alternative to the alt key to save fingers, as per Item 2 here:
;; https://sites.google.com/site/steveyegge2/effective-emacs
(global-set-key "\C-x\C-m" 'execute-extended-command)

;; to save typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; show bell icon (e.g. on C-g)...  I dislike the visual bell, but don't want a plugin yet
(setq-default visible-bell t)

;; Delete trailing whitespaces, but...
(add-hook 'before-save-hook
          'delete-trailing-whitespace)
;; ... Don't delete trailing newlines (since opinions differ on this)
(setq-default delete-trailing-lines nil)

;; modeline
(line-number-mode t)
(column-number-mode t)

;; show line numbers in buffers where we're editing text or code
(defvar rk/display-line-numbers-excluded-modes
  '(term-mode ansi-term-mode eshell-mode shell-mode vterm-mode treemacs-mode help-mode
              compilation-mode)
  "Major modes where line numbers stay disabled.")

(defun rk/enable-line-numbers-when-editing ()
  "Turn on `display-line-numbers-mode' for buffers derived from `prog-mode' or `text-mode'."
  (when (and (not (minibufferp))
             (derived-mode-p 'prog-mode 'text-mode)
             (not (memq major-mode rk/display-line-numbers-excluded-modes)))
    (display-line-numbers-mode 1)))

(add-hook 'after-change-major-mode-hook #'rk/enable-line-numbers-when-editing)

;; trunate lines and wrap words in org mode
(add-hook 'org-mode-hook 'visual-line-mode)

;; Ret to follow links in org mode
(setq org-return-follows-link t)

;; Otherwise emacs only treats ".  "  as end of sentence
(setq-default sentence-end-double-space nil)

;; Delete selected text if you type while it is highlighted
(delete-selection-mode t)

;; no thanks to intrusive emacs project pop-ups
(defalias 'view-emacs-news 'ignore)
(defalias 'describe-gnu-project 'ignore)
(defalias 'describe-copying 'ignore)

;; automatically add delete closing parens
(electric-pair-mode t)

(show-paren-mode t) ;; highlight matching parens

(save-place-mode 1) ;; save cursor position in file

(setq-default dired-listing-switches "-alh") ;; human readable file sizes in dired

;; Revert buffer to disk version if it has changed
(global-auto-revert-mode 1)
;; And for dired
(add-hook 'dired-mode-hook #'auto-revert-mode)
(setq auto-revert-verbose nil   ; hide the echo‑area messages
      global-auto-revert-non-file-buffers t
      auto-revert-use-notify t)

;;; Preserve contents of system clipboard
(setq save-interprogram-paste-before-kill t)

;; remember which files were last opened
;; use recentf-open-files to view them
(recentf-mode t)

;; show clock
(display-time-mode t)
;; show 24 hr time
(setq-default display-time-24hr-format t)

;; Treat wordsSeparatedByCapitalLetters as separate words
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'gptel-mode-hook 'subword-mode)
(add-hook 'typescript-mode-hook 'subword-mode)
(add-hook 'typescript-ts-mode-hook 'subword-mode)
(add-hook 'magit-mode-hook 'subword-mode)

;; Hide ugly newline arrows in fringe
(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil nil) ;; no continuation indicators
      ;; '(nil right-curly-arrow) ;; right indicator only
      ;; '(left-curly-arrow nil) ;; left indicator only
      ;; '(left-curly-arrow right-curly-arrow) ;; default
      )
;; show errors, linting etc in left column
(fringe-mode)

;; Repeat C-u C-<SPC> and C-x C-<SPC> by just using C-<SPC> after first jump
(setq set-mark-command-repeat-pop 1)

;; highlight TODO
(add-hook 'prog-mode-hook 'hl-todo-mode)
(add-hook 'web-mode-hook 'hl-todo-mode)
(add-hook 'markdown-mode-hook 'hl-todo-mode)
(add-hook 'fundamental-mode-hook 'hl-todo-mode)

;; word-wrap in markdown mode, text mode, etc
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'fundamental-mode-hook 'visual-line-mode)


;; Add vterm / magit to project switch modeline
;; Wrapped like this to prevent things breaking - this pattern was new to me
;; and feels smart. Should almost certainly be using it elsewhere!
(with-eval-after-load 'project
  (add-to-list 'project-switch-commands '(?t "vterm" vterm))
  (define-key project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  )

;; Bigger scrollback
(setq vterm-max-scrollback 200000)

;; ignore directories when grepping in a project - doesn't work for C-u C-x p g
(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories "dist")
     (add-to-list 'grep-find-ignored-directories "build")
     (add-to-list 'grep-find-ignored-files "package-lock.json")))
;; Truncate lines in grep results
(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))

(add-hook 'prog-mode-hook 'hs-minor-mode)

;; smooth scrolling
(pixel-scroll-precision-mode 1)

;; M-3 to insert #
;; Since using a British keyboard layout, I have to press Alt-3 to get a hash
;; Note that C-3 achieves the same as M-3 (i.e. C-u 3)
(global-set-key (kbd "M-3") (lambda () (interactive) (insert "#")))

;; Use regexp search by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Use ripgrep, not grep
(setq grep-program "rg")
(setq xref-search-program 'ripgrep) ;; Ensures `project-find-regexp` uses `rg`























;; ============= MAPPINGS / FUNCTIONS =============

(with-eval-after-load 'org
  ;; Define a function to jump to the next unchecked checkbox
  (defun org-next-unchecked-checkbox ()
    "Jump to the next unchecked checkbox at any level."
    (interactive) ;; Make the function interactive
    (re-search-forward "^[ \t]*- \\[ \\]" nil t)) ;; Search for next '- [ ]', accounting for nesting
  (define-key org-mode-map (kbd "C-c C-n") 'org-next-unchecked-checkbox)
  ;; Define a function to jump to the previous unchecked checkbox
  (defun org-previous-unchecked-checkbox ()
    "Jump to the previous unchecked checkbox at any level."
    (interactive) ;; Make the function interactive
    (re-search-backward "^[ \t]*- \\[ \\]" nil t)) ;; Search for previous '- [ ]', accounting for nesting
  (define-key org-mode-map (kbd "C-c C-p") 'org-previous-unchecked-checkbox))

;; Utility function for creating a binding for a vterm buffer with a given name
(defun open-custom-vterm (buffer-name command-list)
  "Open a custom vterm with a given BUFFER-NAME and execute a list of COMMAND-LIST."
  (let ((target-buffer (get-buffer buffer-name)))
    (if target-buffer
        ;; If the buffer exists, switch to it.
        (switch-to-buffer target-buffer)
      ;; Else, create a new vterm and rename the buffer.
      (vterm)
      (rename-buffer buffer-name t)))
  ;; Iterate over the command list and send each command to the terminal.
  (dolist (command command-list)
    (vterm-send-string command)
    (vterm-send-return)))
;; Example usage for a new function
;; (defun open-logging-terminal ()
;;   "Open a logging terminal for project A."
;;   (interactive)
;;   (open-custom-vterm "test-project-A-terminal"
;;                      '("cd ~/path/to/project-a" "npm run test --watch")))
;; then set to a keybinding
;; (global-set-key (kbd "C-c L") 'open-logging-terminal)

;; Use our util functions to create a function to open an ssh terminal
(defun open-ssh-terminal ()
  "Open an SSH terminal with a predefined SSH command loaded from a separate file."
  (interactive)
  (let ((cmd (when (boundp 'ssh-command) ssh-command)))
    (open-custom-vterm "ssh-terminal" (list (or cmd "ssh user@host")))))
(global-set-key (kbd "C-c S") 'open-ssh-terminal)

;; Run nchat in a specially named vterm
(defun open-nchat-terminal ()
  "Open a vterm to run nchat."
  (interactive)
  (open-custom-vterm "nchat" '("nchat")))
(global-set-key (kbd "C-c n") 'open-nchat-terminal)

;; for convenient hiding
(global-set-key (kbd "C-c h") 'hs-hide-level)


(defun open-org-file ()
  (interactive)
  (find-file "~/notes/org.org"))
(global-set-key (kbd "C-c o") 'open-org-file)
(defun open-corg-file ()
  (interactive)
  (find-file "~/notes/corg.org"))
(global-set-key (kbd "C-c O") 'open-corg-file)

;; C-c r to open record file
(defun open-record ()
  (interactive)
  (find-file "~/notes/record.org"))
(global-set-key (kbd "C-c r") 'open-record)


;; vterm - creates / switches to buffer called 'vterm'
;; can rename each vterm buffer with incrementing suffix using C-x x u
;; Doing so allows you to open a new vterm buffer with C-c t
(global-set-key (kbd "C-c t") 'vterm)



;; org mode add a day as header to next line, then move point under it, for logging activity
(fset 'org-new-day
      (kmacro-lambda-form [?\M-< return ?\C-p ?* ?  ?\C-c ?. ?  return return] 0 "%d"))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c a d") 'org-new-day))

;; replace (e.g.) "should return" with "returns" - e.g. in test it("") statements
(fset 'should-be-gone
   (kmacro-lambda-form [?\M-< ?\M-< ?\S-\C-\M-s ?\[ ?\' ?\\ ?| ?\" ?\\ ?\] ?s ?h ?o ?u ?l ?d return M-backspace ?\C-d ?\M-f ?s ?\C-n ?\C-a] 0 "%d"))

(fset 'convert-typescript-class-function-to-arrow-function
   (kmacro-lambda-form [?\C-s ?\( return ?\C-b ?  ?= ?  ?\C-e ?\C-b ?= ?> ?  ?\C-a ?\C-n ?\S-\C-\M-s ?p ?r ?i ?v ?a ?t ?e ?\\ ?| ?p ?u ?b ?l ?i ?c return ?\C-a] 0 "%d"))



;; for mac os x https://www.emacswiki.org/emacs/FullScreen#h5o-27
(defun toggle-fullscreen ()
  ;; "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;; backup / autosave / lock files - don't litter directories
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves" t)))
(setq create-lockfiles nil)

;; No tabs by default
(setq-default indent-tabs-mode nil)

;; For commit message, grab ticket number from branch name and insert at top of
;; commit message, if the ticket number is available
;; e.g. "feature/aff-1234-foo" -> "AFF-1234: "
(defun insert-ticket-number ()
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (when (string-match "[a-zA-Z]+-[0-9]\\{1,\\}" branch)
      (let ((prefix (upcase (match-string 0 branch))))
        (unless (string-prefix-p prefix (buffer-string))
          (insert prefix ": "))))))

;; On opening magit commit message, insert ticket number
(add-hook 'git-commit-setup-hook 'insert-ticket-number)

;; expand region stuff (there's more, but this is all I need)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)

;; C-c m to open *messages*, useful for looking up errors
(defun open-messages ()
  (interactive)
  (switch-to-buffer "*Messages*"))
(global-set-key (kbd "C-c m") 'open-messages)

;; C-c M to yank last message - useful when wanting to google (say) a linting or lang server
;; error currently displayed in the minibuffer
(defun copy-last-message-to-clipboard ()
  (interactive)
  (let ((current-buffer (current-buffer)))
    (with-current-buffer "*Messages*"
      ;; Step 1: Already in "*Messages*" buffer due to 'with-current-buffer'
      ;; Step 2: Go to the end of the buffer
      (goto-char (point-max))
      ;; Step 3: Move to the beginning of the previous line
      (forward-line -1)
      (move-beginning-of-line nil)
      ;; Step 4: Save the beginning of the line
      (let ((beg (point)))
        ;; Step 5: Copy the previous line to the clipboard
        (move-end-of-line nil)
        (kill-new (buffer-substring-no-properties beg (point)))))
    ;; Step 6: Return to the previous buffer
    (switch-to-buffer current-buffer)))
(global-set-key (kbd "C-c M") 'copy-last-message-to-clipboard)

;; C-c L to load-file ~/.emacs
(defun load-emacs ()
  (interactive)
  (load-file "~/.emacs"))
(global-set-key (kbd "C-c l") 'load-emacs)

;; C-c b to bury buffer
(global-set-key (kbd "C-c b") 'bury-buffer)

;; C-c e  open ~/.emacs
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs")))
;; override org-mode binding for C-c e
;; (define-key org-mode-map (kbd "C-c e")
;;   (lambda () (interactive) (find-file d"~/.emacs")))

;; C-c s to open scratch buffer
(global-set-key (kbd "C-c s") 'switch-to-scratch-buffer)
;; switch to scratch buffer
(defun switch-to-scratch-buffer ()
  "Switch to the scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

;; Org mode by default in scratch buffer
(setq initial-major-mode 'org-mode)

;; display full file path in modeline
(setq-default mode-line-buffer-identification
              '(:eval (if buffer-file-name
                          (abbreviate-file-name buffer-file-name)
                        "%b")))

(defvar vterm-search-string "robert.kirby@ssg"
  "String to search for in vterm buffer.")
(defun vterm-copy-previous-output (arg)
  "Copy from the ARG+2th most recent occurrence of `vterm-search-string' to the end of the buffer in vterm copy mode.
If ARG is not provided, copy from the second most recent occurrence."
  (interactive "p")
  (require 'vterm)
  ;; Enable vterm copy mode
  (vterm-copy-mode 1)
  ;; Jump to the ARG+2th most recent occurrence of `vterm-search-string'
  (let ((num-found (how-many vterm-search-string (point-min) (point-max))))
    (when (> num-found (+ arg 1))
      (dotimes (_ (+ arg 1))
        (re-search-backward vterm-search-string))))
  ;; Start marking
  (set-mark (point))
  ;; Jump to the end of the line before the last occurrence
  (goto-char (point-max))
  (re-search-backward vterm-search-string)
  (beginning-of-line)
  ;; Copy to clipboard
  (kill-ring-save (mark) (point))
  ;; Exit vterm copy mode
  (vterm-copy-mode -1))
;; Bind the new function to the key sequence C-c O (O for Output)
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-c O") 'vterm-copy-previous-output))
(defun my/switch-to-vterm (num)
  "Switch to vterm buffer NUM or create it if it doesn't exist."
  (let* ((buffer-name (format "*vterm*<%d>" num))
         (buffer (get-buffer buffer-name)))
    (if buffer
        (switch-to-buffer buffer)
      (vterm)
      (rename-buffer buffer-name t))))
(defun my/rename-vterm-buffer (num)
  "Rename current vterm buffer to *vterm*<NUM>."
  (interactive "nRename vterm buffer to number: ")
  (let ((new-name (format "*vterm*<%d>" num)))
    (rename-buffer new-name t)
    (message "Buffer renamed to %s" new-name)))
;; Bind C-c v 1..9 to switch/create vterm buffers
(dotimes (i 9)
  (let ((n (1+ i)))
    (global-set-key (kbd (format "C-c v %d" n))
                    `(lambda () (interactive) (my/switch-to-vterm ,n)))))
;; Bind C-c v r to rename current vterm buffer to a number
(global-set-key (kbd "C-c v r") #'my/rename-vterm-buffer)
(defun my/vterm-buffer-numbers ()
  "Return sorted list of existing numbered vterm buffers."
  (sort (delq nil
              (mapcar (lambda (buf)
                        (when (string-match "\\*vterm\\*<\\([0-9]+\\)>" (buffer-name buf))
                          (string-to-number (match-string 1 (buffer-name buf)))))
                      (buffer-list)))
        #'<))

(defun my/vterm-next-buffer ()
  "Switch to the next numbered vterm buffer, skipping gaps."
  (interactive)
  (let* ((numbers (my/vterm-buffer-numbers)))
    (if numbers
        (let* ((current (when (string-match "\\*vterm\\*<\\([0-9]+\\)>" (buffer-name))
                          (string-to-number (match-string 1 (buffer-name)))))
               (next (or (seq-find (lambda (n) (and current (> n current))) numbers)
                         (car numbers))))
          (switch-to-buffer (format "*vterm*<%d>" next)))
      (message "No numbered vterm buffers available"))))

(defun my/vterm-prev-buffer ()
  "Switch to the previous numbered vterm buffer, skipping gaps."
  (interactive)
  (let* ((numbers (reverse (my/vterm-buffer-numbers))))
    (if numbers
        (let* ((current (when (string-match "\\*vterm\\*<\\([0-9]+\\)>" (buffer-name))
                          (string-to-number (match-string 1 (buffer-name)))))
               (prev (or (seq-find (lambda (n) (and current (< n current))) numbers)
                         (car numbers))))
          (switch-to-buffer (format "*vterm*<%d>" prev)))
      (message "No numbered vterm buffers available"))))
;; Alias bindings for convenience
(global-set-key (kbd "C-c >") #'my/vterm-next-buffer)
(global-set-key (kbd "C-c <") #'my/vterm-prev-buffer)




;; Compilation mode
(defun universal-test-command ()
  "Run tests based on the project type."
  (interactive)
  (let ((default-directory (or (locate-dominating-file default-directory "go.mod")
                               (locate-dominating-file default-directory "package.json")
                               (locate-dominating-file default-directory "requirements.txt")
                               default-directory)))
    (cond
     ((file-exists-p (expand-file-name "go.mod" default-directory))
      (compile "staticcheck ./..."))
     ((file-exists-p (expand-file-name "package.json" default-directory))
      (compile "npm test"))
     ((file-exists-p (expand-file-name "requirements.txt" default-directory))
      (compile "pytest"))
     (t (message "Unknown project type.")))))
;; Butter fingers
(global-set-key (kbd "<f2>") 'universal-test-command)
(global-set-key (kbd "<f3>") 'universal-test-command)
(global-set-key (kbd "<f4>") 'universal-test-command)
(global-set-key (kbd "<f5>") 'universal-test-command)
(global-set-key (kbd "<f6>") 'universal-test-command)
(global-set-key (kbd "<f7>") 'universal-test-command)
(global-set-key (kbd "<f8>") 'universal-test-command)

(global-set-key (kbd "C-c w") 'toggle-word-wrap)

(with-eval-after-load 'org
  (defun rk/org-toggle-then-next ()
    "Toggle the checkbox on this line (via `org-toggle-checkbox`), then
move to the next line and place point at the first non-whitespace char."
    (interactive)
    (save-excursion
      (beginning-of-line)
      (when (org-at-item-checkbox-p)
        (org-toggle-checkbox)))
    (forward-line 1)
    (back-to-indentation))
  ;; Org-only binding for quick checkbox toggle + advance
  (define-key org-mode-map (kbd "C-c c") #'rk/org-toggle-then-next))

;; --- Investigation helpers for LLM harnesses -------------------------------

(defun rk/--maybe-send-or-insert (text)
  "If in a vterm buffer, bracket-paste TEXT; otherwise insert it."
  (if (derived-mode-p 'vterm-mode)
      (vterm-send-string text t)        ; t ⇒ use bracketed paste
    (insert text)))









;; ============= PACKAGES / PLUGINS / EXTENSIONS =============

;; Use Homebrew's GNU ls (gls)
;; On a mac you'll need to run `brew install coreutils` in the terminal first
(when (executable-find "gls")
  (setq insert-directory-program (executable-find "gls")
        dired-use-ls-dired t
        dired-listing-switches "-alh --group-directories-first"))

;; outsource autoindentation to dtrt-indent, as it's a pain
(dtrt-indent-global-mode t)

;; pop-up showing next possible key press - similar natively by '[keypress] ?'
(which-key-mode)
(setq which-key-idle-delay 0.5)

(global-set-key (kbd "C-c f") 'ffap)

;; magit ease-of-use bindings
(global-set-key (kbd "C-c g d") 'magit-diff-buffer-file) ;; diff current buffer

;; always use dired
(global-set-key (kbd "C-x C-d") 'dired)

(setq auto-mode-alist
      (append '(
                ("\\.txt\\'" . text-mode)

                ("\\.mk\\'" . makefile-mode)
                ("Makefile\\'" . makefile-mode)

                ("\\.sh\\'" . shell-script-mode)
                ("\\.trivyignore\\'" . shell-script-mode)
                ("\\.zshrc.rob-universal\\'" . shell-script-mode)
                ("\\.env\\(\\..*\\)?\\'" . shell-script-mode)
                ("\\..*ignore\\'" . shell-script-mode)
                ("\\.nvmrc\\'" . shell-script-mode)
                )
              auto-mode-alist))

;; Enable flashing mode-line on errors
(with-eval-after-load 'doom-themes
  (doom-themes-visual-bell-config))

;; This will probably be included in a new version of emacs, in filenotify.el
;; https://www.blogbyben.com/2022/05/gotcha-emacs-on-mac-os-too-many-files.html
;; When you see the warning "Too many files open", run this
(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

;; Color theme
(unless (package-installed-p 'ef-themes)
  (package-install 'ef-themes))
(load-theme 'ef-light t)

;; Alias for grip-mode, which toggles in-browser md previews - since I can never remember the name
(defun preview-markdown ()
  (interactive)
  (if (bound-and-true-p grip-mode)
      (grip-mode -1)
    (grip-mode 1)
    )
  )

(load-file "~/dotfiles/accumulate-text.el")





;; --- START OF LANGUAGE SERVER STUFF ----
;; Useful defaults - eglot stuff via xref:
;; M-. for go to def
;; M-, travel back in time through def jumps (akin to C-x C-space)
;; Also, custom flymake:
;; M-n go to next error in file
;; M-p go to prev error in file
;; Start Eglot automatically in TS/TSX
;; 1) Sources (keep this so future installs/updates work)
(setq treesit-language-source-alist
      '((javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
        (tsx         "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
        (csv "https://github.com/tree-sitter-grammars/tree-sitter-csv" nil "csv/src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (css  "https://github.com/tree-sitter/tree-sitter-css")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")))

;; 2) Prefer Tree-sitter modes and file associations
(add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
(add-to-list 'major-mode-remap-alist '(js-jsx-mode     . tsx-ts-mode))
(add-to-list 'major-mode-remap-alist '(js-mode         . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode     . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(json-mode       . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'"   . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'"  . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"   . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'"  . js-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'"   . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.csv\\'"  . csv-mode))

;; 3) Auto-start Eglot in modes backed by tsserver (TS/TSX/JS/JSON)
(dolist (mode '(typescript-ts-mode
                tsx-ts-mode
                js-ts-mode
                json-ts-mode))
  (add-hook (intern (format "%s-hook" mode)) #'eglot-ensure))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((bash-ts-mode sh-mode) . ("bash-language-server" "start"))))

(setq major-mode-remap-alist
      '((html-mode       . html-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (js-jsx-mode     . tsx-ts-mode)
        (python-mode     . python-ts-mode)
        (css-mode        . css-ts-mode)
        (json-mode       . json-ts-mode)
        (yaml-mode       . yaml-ts-mode)))

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error))
;; --- END OF LANGUAGE SERVER STUFF ----


;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
