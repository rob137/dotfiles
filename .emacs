;; ============= PACKAGE MANAGEMENT =============
;;; Commentary: purely to suppress errors

;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; for making the package exec-path-from-shell work
;; this package allows lsp-mode to find npm when npm is at a non-standard
;; directory - e.g. when npm is installed via nvm (and bit always should be)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(setq exec-path (append exec-path '("/Users/robert.kirby/.n")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(codeium/metadata/api_key "8e92b8d8-76d7-438c-968e-80f8719fb00b")
 '(custom-safe-themes
   '("46c65f6d9031e2f55b919b1486952cddcc8e3ee081ade7eb2ffb6a68a804d30e" "b64a60e69617b4348d0402fad2f0d08a694301132e7ab243dab4d6a65c3bf948" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee" "7e068da4ba88162324d9773ec066d93c447c76e9f4ae711ddd0c5d3863489c52" "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" default))
 '(git-gutter:update-interval 2)
 '(grep-files-aliases
   '(("all" . "* .*")
     ("el" . "*.el")
     ("ch" . "*.[ch]")
     ("c" . "*.c")
     ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++")
     ("cchh" . "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
     ("hh" . "*.hxx *.hpp *.[Hh] *.HH *.h++")
     ("h" . "*.h")
     ("l" . "[Cc]hange[Ll]og*")
     ("am" . "Makefile.am GNUmakefile *.mk")
     ("m" . "[Mm]akefile*")
     ("tex" . "*.tex")
     ("texi" . "*.texi")
     ("asm" . "*.[sS]")
     ("go" . "*.go")
     ("php" . "*.php")
     ("py" . "*.py")
     ("ts" . "*.ts")
     ("js" . "*.js")
     ("css" . "*.css")
     ("html" . "*.html")
     ("json" . "*.json")
     ("jsonl" . "*.jsonl")
     ("jsonld" . "*.jsonld")
     ("sh" . "*.sh")))
 '(org-agenda-files '("~/notes/todo.org"))
 '(package-selected-packages
   '(hl-todo magit sass-mode chatgpt epc ctable concurrent deferred quelpa-use-package quelpa php-mode json-mode jsfmt which-key web-mode vterm typescript-mode tree-sitter-langs scss-mode rjsx-mode restclient prettier-rc prettier lsp-ui jenkinsfile-mode indium highlight grip-mode gotest git-gutter fzf flycheck expand-region exec-path-from-shell evil-collection eslint-rc dtrt-indent doom-themes dockerfile-mode docker-compose-mode coverage cov company-quickhelp auto-org-md))
 '(safe-local-variable-values
   '((eval add-to-list 'grep-find-ignored-files "*.json")
     (vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (cov-lcov-file-name . "/Users/robert.kirby/g/aff-services-mono/apps/bookmaker-aliases-servicex/coverage/unit/lcov.info")
     (cov-lcov-file-name . "/Users/robert.kirby/g/aff-services-mono/apps/bookmaker-aliases-service/coverage/unit/lcov.info")
     (cov-lcov-file-name . "/Users/robert.kirby/g/events-service/coverage/unit/lcov.info")
     (cov-lcov-file-name . "/Users/robert.kirby/g/events-service/coverage/lcov.info")
     (lcov-file-name . "/Users/robert.kirby/g/events-service/coverage/lcov.info"))))











;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; packages installed via straight.el
(straight-use-package '(codeium :type git :host github :repo "Exafunction/codeium.el"))
(straight-use-package '(copilot :type git :host github :repo "zerolfx/copilot.el" :files ("dist" "*.el")))




















;; ============= Emacs NATIVE SETTINGS =============

;; hide ui
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Don't display the 'Welcome to GNU Emacs' buffer on startup
(setq-default inhibit-startup-message t)

;; don't show scratch buffer description text
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

;; add a 5 line buffer between the point and the top / bottom of the window
(setq scroll-margin 5)

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

;; vertical line on 80 char column
(setq-default display-fill-column-indicator-column 79)
(global-display-fill-column-indicator-mode t)

;; easier to find cursor
(global-hl-line-mode 1)

;; an alternative to the alt key to save fingers, as per Item 2 here:
;; https://sites.google.com/site/steveyegge2/effective-emacs
(global-set-key "\C-x\C-m" 'execute-extended-command)

;; to save typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; make git gutter show for files in git repos
(global-git-gutter-mode +1)

;; show bell icon (e.g. on C-g)...  I dislike the visual bell, but don't want a plugin yet
(setq-default visible-bell 1)

;; Delete trailing whitespaces, but...
(add-hook 'before-save-hook
          'delete-trailing-whitespace)
;; ... Don't delete trailing newlines (since opinions differ on this)
(setq-default delete-trailing-lines nil)

;; for node, which is needed for prettier
(add-to-list 'exec-path "/usr/local/bin/node")

;; modeline
(line-number-mode t)
(column-number-mode t)

;; show line numbers in left column when editing code
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'markdown-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'yml-mode-hook 'display-line-numbers-mode)
(add-hook 'docker-compose-mode-hook 'display-line-numbers-mode)

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
(global-auto-revert-mode)
;; refresh dired when files change
;; doesn't seem to have any effext - dired doesn't update
;; might be redundant because of above line
(setq-default dired-auto-revert-buffer t)

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
(add-hook 'go-mode-hook 'subword-mode)
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
(setq vterm-max-scrollback 100000000)

;; ignore directories when grepping in a project - doesn't work for C-u C-x p g
(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories "dist")
     (add-to-list 'grep-find-ignored-directories "build")
     (add-to-list 'grep-find-ignored-directories "package-lock.json")))
;; Truncate lines in grep results
(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))

(add-hook 'prog-mode-hook 'hs-minor-mode)
(global-set-key (kbd "C-c @ @") 'hs-toggle-hiding)

;; smooth scrolling
(pixel-scroll-precision-mode 1)

;; M-3 to insert #
;; Since using a British keyboard layout, I have to press Alt-3 to get a hash
;; Note that C-3 achieves the same as M-3 (i.e. C-u 3)
(global-set-key (kbd "M-3") (lambda () (interactive) (insert "#")))

;; Use regexp search by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
























;; ============= MAPPINGS / FUNCTIONS =============

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
    (vterm-send-string (concat command "\n"))
    (vterm-send-return)))
;; Example usage for a new function
;; (defun open-logging-terminal ()
;;   "Open a logging terminal for project A."
;;   (interactive)
;;   (open-custom-vterm "test-project-A-terminal"
;;                      '("cd ~/path/to/project-a" "npm run test --watch")))
;; then set to a keybinding
;; (global-set-key (kbd "C-c L") 'open-logging-terminal)

;; Load ssh terminal setup (deliberately not tracked in source control)
(load "~/.ssh-command.el")  ;; Adjust the path to where you've saved ssh-command.el
;; Use our util functions to create a function to open an ssh terminal
(defun open-ssh-terminal ()
  "Open an SSH terminal with a predefined SSH command loaded from a separate file."
  (interactive)
  (open-custom-vterm "ssh-terminal" (list ssh-command)))
(global-set-key (kbd "C-c S") 'open-ssh-terminal)

;; Run AICE in a specially named vterm
(defun open-aice-server-terminal ()
  "Open a vterm to run AICE."
  (interactive)
  (open-custom-vterm "aice-server-terminal"
                     '("cd ~/g/aice" "make run")))
(global-set-key (kbd "C-c A") 'open-aice-server-terminal)

;; for convenient hiding
(global-set-key (kbd "C-c h") 'hs-hide-level)

;; C-c o to open org mode file
(defun open-todo ()
  (interactive)
  (find-file "~/notes/org.org"))
(global-set-key (kbd "C-c o") 'open-todo)

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



;; for autoformatting code with prettier
;; prettify highlighted text, or the whole file
(defun prettify ()
  (interactive)
  (if (region-active-p)
      (prettier-prettify-region)
    (prettier-prettify)))
(global-set-key (kbd "C-c p") 'prettify) ;; probably shrould be a single binding for all formatters, which listens for correct file type...

;; Python Black - similar to prettier
;; Reformat buffer or region
(defun python-black-format ()
  (interactive)
  (if (region-active-p)
      (python-black-region)
    (python-black-buffer)))

;; PHP format on save ith intelliphense via eglot
(add-hook 'php-mode-hook
    (lambda ()
      (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
(defalias 'add-php-opening-tag-to-current-file
   (kmacro "C-x C-j C-n <return> M-< C-s F o r m R e q <return> C-a C-SPC C-e <backspace> <backspace> <backspace> C-a C-x C-s"))



;; use text-mode in blade.php files
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . text-mode))
;; use php mode in blade.php files
;; DISABLED because intelephense ain't all that with blade
;; still ongoing thing: https://github.com/bmewburn/vscode-intelephense/issues/93#issuecomment-1940670756
;; (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . php-mode))

;; Define a function to insert <?php at the start of the PHP buffer
(defun insert-php-tag-at-start ()
  "Insert <?php at the beginning of the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "<?php\n\n")))
(add-hook 'php-mode-hook
  (lambda ()
    (local-set-key (kbd "C-c P") 'insert-php-tag-at-start)))


;; Python-specific keybindings
(defun set-python-mode-keybindings ()
  (local-set-key (kbd "C-c p") 'python-black-format))
(add-hook 'python-mode-hook 'set-python-mode-keybindings)

(global-set-key (kbd "C-c c o") 'compile)

;; Php-specific keybindings
(defun my-php-insert-php-tag ()
  "Insert '<?php' at the start of the file, followed by two newlines, and return to the original cursor position."
  (interactive)
  (when (derived-mode-p 'php-mode) ;; Make sure this only runs in php-mode
    (let ((current-point (point)))   ;; Save the current cursor position
      (goto-char (point-min))        ;; Jump to the start of the file
      (unless (looking-at-p "<?php") ;; Check if '<?php' isn't already at the start
        (insert "<?php\n\n"))        ;; Insert '<?php' and two newlines
      (goto-char current-point))))   ;; Return to the saved cursor position
(add-hook 'php-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c P") 'my-php-insert-php-tag)))


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

;; C-c l to load-file ~/.emacs
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

(defun toggle-typescript-source-and-spec ()
  "Toggle between TypeScript source file and its corresponding .spec file."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (spec-file (replace-regexp-in-string "\.ts$" ".spec.ts" current-file))
         (source-file (replace-regexp-in-string "\.spec\.ts$" ".ts" current-file)))
    (if (string= current-file source-file)
        (find-file spec-file)
      (find-file source-file))))
(with-eval-after-load 'typescript-mode
  (define-key typescript-mode-map (kbd "C-c T") 'toggle-typescript-source-and-spec))

(defun toggle-go-source-and-test ()
  "Toggle between Go source file and its corresponding _test.go file."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (test-file (replace-regexp-in-string "\\.go$" "_test.go" current-file))
         (source-file (replace-regexp-in-string "_test\\.go$" ".go" current-file)))
    (if (string= current-file source-file)
        (find-file test-file)
      (find-file source-file))))
(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-c T") 'toggle-go-source-and-test))

(defun split-window-thirds ()
  "Split a window into thirds."
  (interactive)
  (split-window-right)
  (split-window-right)
  (balance-windows))

(bind-key "C-c 3" #'split-window-thirds)


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
;; Bind the new function to the key sequence C-c O
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-c C") 'vterm-copy-previous-output))

(defun close-other-vterm-buffers ()
  "Close all vterm buffers except the current one if it's a *vterm buffer, or close all but *vterm* otherwise.  Note that buffers that don't start with *vterm won't be deleted - such as 'ssh-terminal'."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (dolist (buffer (buffer-list))
      (when (and (string-prefix-p "*vterm" (buffer-name buffer))
                 (or (not (eq buffer current-buffer))
                     (not (derived-mode-p 'vterm-mode)))
                 (not (string= (buffer-name buffer) "*vterm*")))
        (kill-buffer buffer)))))

(global-set-key (kbd "C-c v 1") 'close-other-vterm-buffers)





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

;; SSG SPECIFIC STUFF

(defun copy-aice-schema-and-notify ()
  "Open vterm, run copy-aice-schema, and notify on completion."
  (interactive)
  ;; Open or switch to a vterm buffer.
  (vterm)
  ;; Clear any unsent commands. Note: Sending C-c C-u to clear the line.
  (vterm-send-C-c)
  (vterm-send-C-u)
  ;; Run the copy-aice-schema command.
  (vterm-send-string "copy-aice-schema")
  (vterm-send-return)
  ;; Wait briefly to ensure the command executes before switching buffers.
  (sleep-for 2)  ;; Adjust the sleep duration as needed.
  ;; Switch back to the previous buffer.
  (previous-buffer)
  ;; Display a message indicating success.
  (message "copy-aice-schema command executed and output copied to clipboard!"))

(global-set-key (kbd "C-c D") 'copy-aice-schema-and-notify)

;; General purpose, for mucking about with local testing
(defun yank-password ()
  "Copy the string 'ABC!123abc' to the clipboard."
  (interactive)
  (let ((password "ABC!123abc"))
    (kill-new password)
    (message "Password copied to clipboard: %s" password)))


;; (defun insert-debug-comment ()
;;   "Insert a debug comment indicating the code should not be committed."
;;   (interactive)
;;   (insert "// DO NOT COMMIT - JUST HERE FOR DEBUGGING PURPOSES"))
;; (global-set-key (kbd "C-c D") 'insert-debug-comment)













;; ============= PACKAGES / PLUGINS / EXTENSIONS =============

;; outsource autoindentation to dtrt-indent, as it's a pain
(dtrt-indent-global-mode t)

;; ;; enable autocompletion, disable by exception
(global-company-mode t)
(add-hook 'org-mode-hook 'company-mode)

;; trying prettier everywhere - note the package is prettier.el, not prettier-js.el
(add-hook 'after-init-hook #'global-prettier-mode)
;; don't open  a new window when we detect syntax errors (it's annoying)
(setq prettier-inline-errors-flag t)

;; Similar to prettier - for python
(add-hook 'python-mode-hook 'python-black-on-save-mode)

;; pop-up showing next possible key press - similar natively by '[keypress] ?'
(which-key-mode)
(setq which-key-idle-delay 0.5)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-javascript-eslint-executable "eslint_d"
      scss-stylelint "stylelint")
(global-set-key (kbd "C-c g n") 'git-gutter:next-hunk)
(global-set-key (kbd "C-c g p") 'git-gutter:previous-hunk)

(global-set-key (kbd "C-c f") 'ffap)

;; magit ease-of-use bindings
(global-set-key (kbd "C-c g d") 'magit-diff-buffer-file) ;; diff current buffer

;; always use dired
(global-set-key (kbd "C-x C-d") 'dired)

;; LSP MODE ---------------------------- Using for now because of eglot mode issues with Gopls, to be fixed in Emacs 29
;; (add-hook 'typescript-mode-hook #'lsp)
;; (add-hook 'js-jsx-mode-hook #'lsp)
;; (add-hook 'html-mode-hook #'lsp)
;; (add-hook 'css-mode-hook #'lsp)
;; For the below:
;; Note that this infers  workspaces based on presence of .git/ directory - which won't always do the trick!
;; See: https://emacs-lsp.github.io/lsp-mode/page/lsp-gopls/#working-with-nested-gomod-files
(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)
(setq lsp-enable-snippet nil)  ;; this prevents weird bug in company-mode, where company mode will insert arguments from function definitions as part of the completion - https://github.com/company-mode/company-mode/issues/943 - I only experienced this with golang
;; Note that this might become unnecessary
;; enable lsp-ui-mode
(setq lsp-enable-symbol-highlighting t)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-doc-show-with-mouse t)
;; mimicking flymake
;; flycheck next error M-n
(global-set-key (kbd "M-n") 'flycheck-next-error)
;;  flycheck previous error M-p
(global-set-key (kbd "M-p") 'flycheck-previous-error)
(global-set-key (kbd "C-c d") 'lsp-find-definition)
(global-set-key (kbd "C-c r") 'lsp-find-references)
(global-set-key (kbd "C-c i") 'lsp-find-implementation)
(global-set-key (kbd "C-c R") 'lsp-rename)
;; END LSP MODE ------------------------

;; EGLOT MODE ----------------------------
;; NOTE Golang / emacs bug due to be fixed in Emacs 29 (probably already fixed in prerelease.. could compile from source)
;; https://github.com/golang/go/issues/54559#issuecomment-1352862969
;; Works perfectly otherwise
;; Flymake goes hand-in-hand with eglot (flycheck plays nice with lsp-mode)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(php-mode . ("intelephense" "--stdio")))
  )

(add-hook 'php-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'js-jsx-mode-hook 'eglot-ensure)
(add-hook 'html-mode-hook 'eglot-ensure)
(add-hook 'css-mode-hook 'eglot-ensure)
(add-hook 'sass-mode-hook 'eglot-ensure)
(add-hook 'sass-mode-hook 'flymake-sass-load)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
;; (add-hook 'go-mode-hook 'eglot-ensure) ;; disabled because using lsp-mode
(add-hook 'eglot--managed-mode-hook
          (lambda ()
            (define-key eglot-mode-map (kbd "C-c d") 'xref-find-definitions)
            (define-key eglot-mode-map (kbd "C-c i") 'eglot-find-implementation)
            (define-key eglot-mode-map (kbd "C-c r") 'xref-find-references)
            (define-key eglot-mode-map (kbd "C-c R") 'eglot-rename)
            )
          )
;; ;; * NOT USED because LSP-mode standing in for now *
;; ;; setup eglot with gopls - from gopls docs https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;; (require 'project)
;; (defun project-find-go-module (dir)
;;   (when-let ((root (locate-dominating-file dir "go.mod")))
;;     (cons 'go-module root)))
;; (cl-defmethod project-root ((project (head go-module)))
;;   (cdr project))
;; (add-hook 'project-find-functions #'project-find-go-module)
;; ;; setup gopls for integratioln build tags - might not be needed if dir_locals.el does it's job in pickwise-beffe
;; (setq eglot-workspace-configuration
;;       '((gopls . ((usePlaceholders . t)
;;                   (completeUnimported . t)
;;                   (staticcheck . t)
;;                   (buildFlags . ["-tags=integration"])))))

;; NOT WORKING Set up eslint to play nice with flymake
;; This might actually be ok - flycheck does the trick happily enough via C-c ! n and C-c ! p
(add-hook 'web-mode-hook
  (lambda ()
    (flymake-eslint-enable)))
(add-hook 'typescript-mode-hook
  (lambda ()
    (flymake-eslint-enable)))
(add-hook 'js-jsx-mode-hook
  (lambda ()
    (flymake-eslint-enable)))
(add-hook 'flymake-mode-hook
          (lambda ()
            (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
            (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
            ;; don't make unnecessary imports invisible - suspect doom zenburn will fix this themselves when eglot is merged into emacs
            ;; (set-face-foreground 'eglot-diagnostic-tag-unnecessary-face "#F18C96") ;; not needed in light mode
            ))
;; END EGLOT MODE ------------------------

(setq auto-mode-alist
      (append '(
                ("\\.js\\'" . typescript-mode)
                ("\\.ts\\'" . typescript-mode)
                ("\\.tsx\\'" . js-jsx-mode)
                ("\\.jsx\\'" . js-jsx-mode)

                ;; Wrap words, truncate lines
                ("\\.txt\\'" . visual-line-mode)

                ("\\.go\\'" . go-mode)

                ("\\.mk\\'" . makefile-mode)
                ("Makefile\\'" . makefile-mode)

                ("\\.sh\\'" . shell-script-mode)
                ("\\.trivyignore\\'" . shell-script-mode)
                ("\\.zshrc.rob-universal\\'" . shell-script-mode)
                ("\\.env\\(\\..*\\)?\\'" . shell-script-mode)
                ("\\.\.*ignore\\'" . shell-script-mode)
                ("\\.nvmrc\\'" . shell-script-mode)
                )
              auto-mode-alist))

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

(fset 'next-theme
      (kmacro-lambda-form [?\C-x ?r ?b ?. ?e ?m ?a tab return ?\M-< ?\C-s ?d ?o ?o ?m ?- ?\C-s ?\C-s ?\M-d ?\C-/ ?\M-x ?z ?p backspace ?a ?p ?- ?u ?p ?- ?t ?o ?- ?c ?h ?h ?a ?r backspace backspace backspace ?a ?r return ?  ?\C-x ?\C-f ?~ ?/ ?s ?a ?n ?d ?p ?i ?t ?/ ?t ?h ?e ?m ?e ?s ?. ?t ?x ?t return ?\M-< ?\C-s ?\C-y return ?\C-n ?\C-a ?\C-s ?- return ?\C-k ?\C-/ ?\C-x ?b return ?\C-y ?\C-e ?\C-x ?\C-e] 0 "%d"))

;; (global-set-key (kbd "C-c n") 'next-theme) ;; sometimes tripped up by this, fun as it was

;; COPILOT
(global-copilot-mode t)
(setq copilot-max-char 200000)
(global-set-key (kbd "C-<tab>") 'copilot-accept-completion) ;; might clash?  experimenting
(global-set-key (kbd "C-c c a") 'copilot-accept-completion)
(global-set-key (kbd "C-c c w") 'copilot-accept-completion-by-word)
(global-set-key (kbd "C-c c l") 'copilot-accept-completion-by-line)
(global-set-key (kbd "C-c c n") 'copilot-next-completion)
(global-set-key (kbd "C-c c p") 'copilot-previous-completion)
(global-set-key (kbd "C-c c d") 'copilot-diagnose)
(global-set-key (kbd "C-c c c") 'copilot-clear-overlay)
(global-set-key (kbd "C-c c D") (lambda () (interactive) (copilot-mode -1) (copilot-mode 1) (copilot-diagnose)))

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

;; Shortcuts for common go-test invocations.
(add-hook 'go-mode-hook (lambda ()
  (company-mode) ; enable company upon activating go

  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; Code layout.
  (setq tab-width 2 indent-tabs-mode 1) ; std go whitespace configuration
  (add-hook 'before-save-hook 'gofmt-before-save) ; run gofmt on each save

  ;; Shortcuts for common go-test invocations.
  (let ((map go-mode-map))
    (define-key map (kbd "C-c p") 'gofmt)
    (define-key go-mode-map (kbd "C-c B") 'break-up-golang-args)
    )
  ))

;; C-c B in go-mode to break up next single line arg list in current file
;; Gofmt doesn't break up long lines; typically result of long function arg lists
(defun break-up-golang-args ()
  "Break up long argument lists in Go function calls and apply gofmt."
  (interactive)
  (if (not (eq major-mode 'go-mode))
      (message "Not in Go mode")
    (let ((current-line (line-number-at-pos))
          (start nil)
          (end nil))
      (save-excursion
        (beginning-of-line)
        (when (setq start (search-forward "(" (line-end-position) t))
          (when (setq end (scan-sexps (1- start) 1))
            (if (not (= current-line (line-number-at-pos end)))
                (message "Brackets not on the same line")
              (let ((arg-string (buffer-substring-no-properties start (- end 1)))
                    (new-arg-string "")
                    (stack '())
                    (in-string nil))
                (dolist (char (string-to-list arg-string))
                  (cond ((= char ?\")
                         (setq in-string (not in-string)))
                        ((and (not in-string) (= char ?\())
                         (push "(" stack))
                        ((and (not in-string) (= char ?\)))
                         (pop stack)))
                  (setq new-arg-string (concat new-arg-string (string char)))
                  (when (and (not in-string) (null stack) (= char ?,))
                    (setq new-arg-string (concat new-arg-string "\n"))))
                (delete-region (1- start) end)
                (insert (concat "(\n" new-arg-string ",\n)"))
                (if (fboundp 'gofmt)
                    (gofmt)
                  (message "gofmt not found. Make sure the Go Emacs package is installed."))))))))))

;; Color theme
(load-theme 'ef-light t)

;; Alias for grip-mode, which toggles in-browser md previews - since I can never remember the name
(defun preview-markdown ()
  (interactive)
  (if (bound-and-true-p grip-mode)
      (grip-mode -1)
    (grip-mode 1)
    )
  )

;; LLM Enhancements
;; Helpers for pulling text into GPTel chat windows
(require 'corsair)
(global-set-key (kbd "C-c g c") 'corsair-open-chat-buffer)
(global-set-key (kbd "C-c g a c") 'corsair-accumulate-file-path-and-contents)
(global-set-key (kbd "C-c g a n") 'corsair-accumulate-file-name)
(global-set-key (kbd "C-c g a v") 'corsair-accumulate-file-path)
(global-set-key (kbd "C-c g a w") 'corsair-accumulate-selected-text)
(global-set-key (kbd "C-c g a D") 'corsair-drop-accumulated-buffer)
(global-set-key (kbd "C-c g f") 'corsair-insert-file-or-folder-contents)


;; gptel default model gpt-4o
(setq gptel-model "gpt-4o")
;; Set the default GPTel session
(setq gptel-default-session "Chat")
(load-file "~/dotfiles/accumulate-text.el")
(load-file "~/.emacs.d/openai-init.el") ;; sets api key with (setq gptel-api-key "secret")
;; (load-file "~/.emacs.d/gemini-init.el") ;; Breaking on latest gptel as at 5 Dec 2024
;; --- end gptel ---

;; For very large files - offered as option in minibuffer when you go to open file over 10mb in size
;; Emacs barely usable for big files
(require 'vlf-setup)

;; Debugging!
(setq dap-auto-configure-features '(sessions locals controls tooltip))







(provide '.emacs)
;;; .emacs ends here








