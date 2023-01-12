;; ============= PACKAGE MANAGEMENT =============

;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; for making the package exec-path-from-shell work
;; this package allows lsp-mode to find npm when npm is at a non-standard
;; directory - e.g. when npm is installed via nvm (and it always should be)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(setq exec-path (append exec-path '("/Users/robert.kirby/.n")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:update-interval 2)
 '(org-agenda-files '("~/notes/todo.org"))
 '(package-selected-packages
   '(sass-mode chatgpt epc ctable concurrent deferred quelpa-use-package quelpa php-mode json-mode jsfmt zenburn-theme which-key web-mode vterm typescript-mode tree-sitter-langs scss-mode rjsx-mode rg restclient prettier-rc prettier magit lsp-ui jenkinsfile-mode indium hl-todo highlight grip-mode gotest git-gutter fzf flycheck expand-region exec-path-from-shell evil-collection eslint-rc eglot dtrt-indent doom-themes dockerfile-mode docker-compose-mode docker coverage cov company-quickhelp auto-org-md))
 '(package-selected-packagesn
   '(grip-mode eglot vterm zenburn-theme gotest dtrt-indent highlight cov coverage indium restclient which-key docker dockerfile-mode doom-themes go-mode jenkinsfile-mode evil-collection evil expand-region hl-todo flycheck eslint-rc prettier-rc scss-mode exec-path-from-shell web-mode docker-compose-mode lsp-mode rg rjsx-mode typescript-mode tree-sitter-langs tree-sitter projectile fzf counsel ag neotree git-gutter vimrc-mode ranger magit prettier))
 '(safe-local-variable-values
   '((vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (cov-lcov-file-name . "/Users/robert.kirby/g/aff-services-mono/apps/bookmaker-aliases-servicex/coverage/unit/lcov.info")
     (cov-lcov-file-name . "/Users/robert.kirby/g/aff-services-mono/apps/bookmaker-aliases-service/coverage/unit/lcov.info")
     (cov-lcov-file-name . "/Users/robert.kirby/g/events-service/coverage/unit/lcov.info")
     (cov-lcov-file-name . "/Users/robert.kirby/g/events-service/coverage/lcov.info")
     (lcov-file-name . "/Users/robert.kirby/g/events-service/coverage/lcov.info"))))































;; ============= Emacs NATIVE SETTINGS =============

;; hide ui
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; dired-style buffer menu
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; don't show scratch buffer description text
(setq initial-scratch-message nil)

(set-frame-parameter (selected-frame) 'alpha '(98 . 98)) ;; Make emacs windows transparent

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

;; Don't display the 'Welcome to GNU Emacs' buffer on startup
(setq-default inhibit-startup-message t)

;; instead of icomplete - covers M-x and help too
;; http://xahpmlee.info/emacs/emacs/emacs_icomplete_mode.html
;; Removed ido-mode as it was causing issues with project grep
(fido-mode t)
;; display results in vertical list
(fido-vertical-mode t)

;; ;; show tabs, spaces, newlines etc
(global-whitespace-mode t)
;; Some of these are noisy, so I've disabled them.
;; Others are worse on certain color themes.
(setq-default whitespace-style '(
                                 face ;; necessary for some of the others in this list
                                 trailing
                                 ;; space-before-tab
                                 ;; space-after-tab
                                 ;; indentation ;; noisy in zenburn
                                 ;; tabs ;; noisy in zenburn
                                 ))

;; smooth scroll
(setq scroll-step 1) ;; don't see difference

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

;;leauu always show current keys (0.01 is dealy - but 0 would show none)
(setq echo-keystrokes 0.01)

;; Delete trailing whitespaces, but...
(add-hook 'before-save-hook
          'delete-trailing-whitespace)
;; ... Don't delete trailing newlines
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

;; automatically add delete closing parens
(electric-pair-mode t)

(show-paren-mode t) ;; highlight matching parens (not sure this changes anything)

(save-place-mode 1) ;; save cursor position in file

(setq-default dired-listing-switches "-alh") ;; human readable sizes in dired


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
(display-time-mode 1)
;; show 24 hr time
(setq-default display-time-24hr-format t)

;; start fiel search from home directory
;; (setq default-directory "~/")

;; Treat wordsSeparatedByCapitalLetters as separate words
(add-hook 'prog-mode-hook 'subword-mode)
;; same but for magit
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

;; highlight TODO
(add-hook 'prog-mode-hook 'hl-todo-mode)
(add-hook 'web-mode-hook 'hl-todo-mode)
(add-hook 'markdown-mode-hook 'hl-todo-mode)
(add-hook 'fundamental-mode-hook 'hl-todo-mode)

;; word-wrap in markdown mode, text mode, etc (experimental)
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'fundamental-mode-hook 'visual-line-mode)

;; Add vterm / magit to project switch modeline
;; Wrapped like this to prevent things breaking - this pattern was new to me
;; and feels smart. Should almost certainly be using it elsewhere!
(with-eval-after-load 'project
  (add-to-list 'project-switch-commands '(?t "vterm" vterm))
  (add-to-list 'project-switch-commands '(?m "magit" magit))
  )

;; ignore directories when grepping in a project - doesn't work for C-u C-x p g
(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories "dist")
     (add-to-list 'grep-find-ignored-directories "build")
     (add-to-list 'grep-find-ignored-directories "package-lock.json")))
;; Truncate lines in grep results
(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))






;; ============= MAPPINGS / FUNCTIONS =============

;; In term mode, make the keys for setting char / line mode *toggle between*
;; char / line mode, saving headaches.
;; Also sets cursor to bar shape in line mode to make the difference clear.
;; I don't love having cursor stuff as a side effect, but there doesn't
;; appear to be a better way to set the cursor shape during term mode.
(require 'term)
(defun term-toggle-mode ()
  (interactive)
  (if (term-in-line-mode)
      (progn (term-char-mode) (setq cursor-type 'box))
    (progn (term-line-mode) (setq cursor-type 'bar))))
(define-key term-mode-map (kbd "C-c C-j") 'term-toggle-mode)
(define-key term-mode-map (kbd "C-c C-k") 'term-toggle-mode)
(define-key term-raw-map (kbd "C-c C-j") 'term-toggle-mode)
(define-key term-raw-map (kbd "C-c C-k") 'term-toggle-mode)

;; C-c o to open org mode file
(defun open-todo ()
  (interactive)
  (find-file "~/notes/org.org"))
(global-set-key (kbd "C-c o") 'open-todo)

;; In term modes, quit the buffer when the process exits
;; (e.g. when you exit the shell)
;; Warning: stolen from stackoverflow...
(defun my-term-handle-exit (&optional process-name msg)
  (message "%s | %s" process-name msg)
  (kill-buffer (current-buffer)))
(advice-add 'term-handle-exit :after 'my-term-handle-exit)

;; Don't prompt me for term type
(defun term-zsh ()
  (interactive)
  (term "/bin/zsh"))

;; vterm - creates / switches to buffer called 'vterm'
;; can rename each vterm buffer with incrementing suffix using C-x x u
;; Doing so allows you to open a new vterm buffer with C-c t
(global-set-key (kbd "C-c t") 'vterm)
;; Rename current buffer to "*vterm*"
(defun rename-buffer-to-*vterm* ()
  (interactive)
  (rename-buffer "*vterm*"))
(global-set-key (kbd "C-x x v") 'rename-buffer-to-*vterm*)

;; org mode fully expand tree under cursor - s-Tab does globally, not always what I want
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c e") 'org-show-subtree))

;; org mode add a day as header to next line, then move point under it, for logging activity
(fset 'org-new-day
      (kmacro-lambda-form [?\C-e return ?* ?* ?  ?\C-c ?. ?  return return] 0 "%d"))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c a d") 'org-new-day))

;; for this at top of TS / JS files, useful for pretty logging:
;; const jl = (x: any) => console.log(JSON.stringify(x, null, 2)); // TODO remove [can ignore this TODO]
(fset 'add-pretty-print-method-to-top-of-file
   (kmacro-lambda-form [?\M-< return return ?\M-< ?c ?o ?n ?s ?t ?  ?j ?l ?  ?= ?  ?\( ?x ?: ?  ?a ?n ?y ?\C-e ?  ?= ?> ?  ?c ?o ?n ?s ?o ?l ?e ?. ?l ?o ?g ?\( ?J ?S ?O ?N ?. ?s ?t ?r ?i ?n ?g ?i ?f ?y ?\( ?x ?, ?  ?n ?u ?l ?l ?, ?  ?2 ?\C-e ?\; ?  ?/ ?/ ?  ?T ?O ?D ?O ?  ?r ?e ?m ?o ?v ?e] 0 "%d"))






;; for autoformatting code with prettier
;; prettify highlighted text, or the whole file
(defun prettify ()
  (interactive)
  (if (region-active-p)
      (prettier-prettify-region)
    (prettier-prettify)))
;; on either condition, I want to append the following to the end:
;; exchange-point-and-mark
;; indent-for-tab-command
;; pop-to-mark-command
(global-set-key (kbd "C-c p") 'prettify) ;; probably shrould be a single binding for all formatters, which listens for correct file type...



(global-set-key (kbd "C-c c o") 'compile)

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

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-c c k") 'kill-compilation)

;; keybinding to open scratch buffer
(global-set-key (kbd "C-c s") 'switch-to-scratch-buffer)
;; switch to scratch buffer
(defun switch-to-scratch-buffer ()
  "Switch to the scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

;; display full file path in modeline
(setq-default mode-line-buffer-identification
              '(:eval (if buffer-file-name
                          (abbreviate-file-name buffer-file-name)
                        "%b")))





;; ============= PACKAGES / PLUGINS =============

;; outsource autoindentation to dtrt-indent, as it's a pain
(dtrt-indent-global-mode t)

;; enable autocompletion, disable by exception
(global-company-mode t)
(defun disable-company-mode ()
  (company-mode -1))
(add-hook 'eshell-mode-hook 'disable-company-mode)
(add-hook 'shell-mode-hook 'disable-company-mode)
(add-hook 'term-mode-hook 'disable-company-mode)
(add-hook 'org-mode-hook 'disable-company-mode)


;; additional package to show documentation alongside autocomplete
;; currently not displaying correctly in html / css files, see:
;; https://github.com/company-mode/company-quickhelp/issues/122
(company-quickhelp-mode)
(setq company-minimum-prefix-length 1
      company-idle-delay 0
      company-selection-wrap-around t
      company-tooltip-flip-when-above t
      ;; select using M-n where n is next to choice
      company-show-quick-access 'left
      ;; To prevent completions always appearing in lowercase - annoying in most languages
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case "keep-prefix"
      )

;; trying prettier everywhere
(add-hook 'after-init-hook #'global-prettier-mode)

;; pop-up showing next possible key press - similar natively by '[keypress] ?'
(which-key-mode)
(setq which-key-idle-delay 0.5)

;; Syntax highlighting - requires major mode to recognise file type, e.g. must
;; first install seperate package typescript-mode.el to see highlighting of .ts
;; files
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

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


;; ;; LSP MODE ---------------------------- probably no longer going to use, but keeping to hand because eglot has issues in go
;; ;; set up the lsp server
;; (add-hook 'typescript-mode-hook #'lsp)
;; (add-hook 'js-jsx-mode-hook #'lsp)
;; (add-hook 'html-mode-hook #'lsp)
;; (add-hook 'css-mode-hook #'lsp)
;; (add-hook 'go-mode-hook #'lsp-deferred)
;; (setq lsp-enable-symbol-highlighting t)
;; (setq lsp-ui-doc-enable t)
;; (setq lsp-ui-doc-show-with-cursor t)
;; (setq lsp-ui-doc-show-with-mouse t)
;; ;; mimicking flymake
;; ;; flycheck next error M-n
;; (global-set-key (kbd "M-n") 'flycheck-next-error)
;; ;;  flycheck previous error M-p
;; (global-set-key (kbd "M-p") 'flycheck-previous-error)
;; (global-set-key (kbd "C-c d") 'lsp-find-definition)
;; ;; END LSP MODE ------------------------

;; EGLOT MODE ----------------------------
;; NOTE Golang / emacs bug due to be fixed in Emacs 29 (probably already fixed in prerelease)
;; https://github.com/golang/go/issues/54559#issuecomment-1352862969
;; Flymake goes hand-in-hand with eglot (flycheck plays nice with lsp-mode)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'js-jsx-mode-hook 'eglot-ensure)
(add-hook 'rjsx-mode-hook 'eglot-ensure)
(add-hook 'html-mode-hook 'eglot-ensure)
(add-hook 'css-mode-hook 'eglot-ensure)
(add-hook 'sass-mode-hook 'eglot-ensure)
(add-hook 'sass-mode-hook 'flymake-sass-load)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'eglot--managed-mode-hook
          (lambda ()
            (define-key eglot-mode-map (kbd "C-c d") 'xref-find-definitions)
            (define-key eglot-mode-map (kbd "C-c r") 'xref-find-references)
            (define-key eglot-mode-map (kbd "C-c R") 'eglot-rename)
            )
          )
;; setup eglot with gopls - from gopls docs https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
(require 'project)
(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))
(cl-defmethod project-root ((project (head go-module)))
  (cdr project))
(add-hook 'project-find-functions #'project-find-go-module)
;; setup gopls for integratioln build tags - might not be needed if dir_locals.el does it's job in pickwise-beffe
(setq eglot-workspace-configuration
      '((gopls . ((usePlaceholders . t)
                  (completeUnimported . t)
                  (staticcheck . t)
                  (buildFlags . ["-tags=integration"])))))
;; We have to set up eslint to play nice with flymake
(add-hook 'web-mode-hook
  (lambda ()
    (flymake-eslint-enable)))
(add-hook 'typescript-mode-hook
  (lambda ()
    (flymake-eslint-enable)))
(add-hook 'flymake-mode-hook
          (lambda ()
            (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
            (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
            ;; don't make unnecessary imports invisible - suspect doom zenburn will fix this themselves when eglot is merged into emacs
            (set-face-foreground 'eglot-diagnostic-tag-unnecessary-face "#F18C96")
            ))
;; END EGLOT MODE ------------------------

(setq auto-mode-alist
      (append '(  ; note these are encapsulated in a '() list
                ("\\.js\\'" . typescript-mode)

                ;; trying this instead of typescript-mode
                ("\\.jsx\\'" . rjsx-mode)
                ("\\.tsx\\'" . rjsx-mode)

                ("\\.ts\\'" . typescript-mode)
                ("\\.tsx\\'" . typescript-mode)

                ;; Wrap words, truncate lines
                ("\\.txt\\'" . visual-line-mode)

                ("\\.go\\'" . go-mode)
                )
              auto-mode-alist))

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

(fset 'next-theme
      (kmacro-lambda-form [?\C-x ?r ?b ?. ?e ?m ?a tab return ?\M-< ?\C-s ?d ?o ?o ?m ?- ?\C-s ?\C-s ?\M-d ?\C-/ ?\M-x ?z ?p backspace ?a ?p ?- ?u ?p ?- ?t ?o ?- ?c ?h ?h ?a ?r backspace backspace backspace ?a ?r return ?  ?\C-x ?\C-f ?~ ?/ ?s ?a ?n ?d ?p ?i ?t ?/ ?t ?h ?e ?m ?e ?s ?. ?t ?x ?t return ?\M-< ?\C-s ?\C-y return ?\C-n ?\C-a ?\C-s ?- return ?\C-k ?\C-/ ?\C-x ?b return ?\C-y ?\C-e ?\C-x ?\C-e] 0 "%d"))

;; (global-set-key (kbd "C-c n") 'next-theme) ;; sometimes tripped up by this, fun as it was


;; copilot, needs the dependencies dash, s, editorconfig, at least it did when I installed it acording to its github README
(load-file "~/.emacs.d/copilot.el/dash.el")
(load-file "~/.emacs.d/copilot.el/copilot.el")
(add-hook 'prog-mode-hook 'copilot-mode)
(add-hook 'web-mode-hook 'copilot-mode)
(global-set-key (kbd "C-c c c") 'copilot-mode) ;; toggle
(global-set-key (kbd "C-c c a") 'copilot-accept-completion)
(global-set-key (kbd "C-c c w") 'copilot-accept-completion-by-word)
(global-set-key (kbd "C-c c l") 'copilot-accept-completion-by-line)
(global-set-key (kbd "C-c c n") 'copilot-next-completion)
(global-set-key (kbd "C-c c p") 'copilot-previous-completion)

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
    (define-key map (kbd "C-c g t p") 'go-test-current-project) ;; current package, really
    (define-key map (kbd "C-c g t f") 'go-test-current-file)
    (define-key map (kbd "C-c g t t") 'go-test-current-test)
    )
  ))

;; Color theme
(require 'doom-themes)
(load-theme 'doom-zenburn t)

;; theme-specific tweaks
;; --- doom-zenburn ---
;; make directories stand out in dired
;; palette: https://en.wikipedia.org/wiki/Wikipedia:Zenburn
(set-face-foreground 'dired-directory "#8CD0D3")
(set-face-bold 'dired-directory t)
(set-face-background 'isearch "gold1")
(set-face-foreground 'lazy-highlight "black")
(set-face-background 'lazy-highlight "grey90")
(set-face-foreground `copilot-overlay-face "#C0BED1")
(set-face-italic `copilot-overlay-face t)
(set-face-background 'icomplete-selected-match "brown4")
(set-face-background 'region "black")

;; TODO make todo items legible
;; --- doom-nord ---
;; Use a different color for the active window's modeline
;; (set-face-background 'mode-line "#3B4252")

;; note for vterm: also depends on having these lines in .zshrc:
;; https://github.com/akermu/emacs-libvterm#shell-side-configuration
;; And some other dependencies that can be installed with homebrew - see github

;; For markdown previewing (grip mode) - C-c C-c g to start/stop grip server
;; This line works fine, except that it produces a warning on startup:
;; ""Symbol's value as variable is void: markdown-mode-command-map""
;; (define-key markdown-mode-command-map (kbd "g") #'grip-mode)

;; Alias for grip-mode, which toggles in-browser md previews - since I can never remember the name
(defun preview-markdown ()
  (interactive)
  (if (bound-and-true-p grip-mode)
      (grip-mode -1)
    (grip-mode 1)
    )
  )














;; ============= EVIL - maybe one day =============
;; (SETQ EVIL-WANT-C-u-scroll t)
;; (setq evil-want-C-w-in-emacs-state t)
;; (setq evil-undo-system 'undo-redo)
;; (require 'evil)
;; (evil-mode nil) ;; decided against this for now
;; (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;; (define-key evil-replace-state-map (kbd "C-g") 'evil-normal-state)
;; (setq evil-normal-state-cursor '(box "cadet blue")
;;       evil-insert-state-cursor '(bar "medium sea green")
;;       evil-emacs-state-cursor '(box "light blue")
;;       evil-visual-state-cursor '(hollow "orange"))
;; (evil-set-leader 'normal (kbd "SPC"))
;; ;; I don't love using vim names like write, but this serves as an example of a leader binding
;; (evil-define-key 'normal 'global (kbd "<leader> w w") 'save-buffer)
;; ;; disable by default
;; ;; overrides - C-z to toggle vim bindings in these modes
;; (evil-set-initial-state 'term-mode 'emacs)
;; (evil-set-initial-state 'esh-mode 'emacs)
;; (evil-set-initial-state 'shell-mode 'emacs)
;; (evil-set-initial-state 'bookmark-bmenu-mode 'emacs)
;; (evil-set-initial-state 'dired-mode 'emacs)
;; (evil-set-initial-state 'outline-mode 'emacs)
;; (evil-set-initial-state 'help-mode 'emacs)
;; (evil-set-initial-state 'calendar-mode 'emacs)
;; (evil-set-initial-state 'buffer-menu-mode 'emacs)
;; (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
;; (evil-set-initial-state 'company-mode 'emacs)
;; (evil-set-initial-state 'fundamental-mode 'emacs)
;; (evil-set-initial-state 'special-mode 'emacs)
;; (evil-set-initial-state 'log-edit-mode 'emacs)
;; (evil-set-initial-state 'vc-log-edit-mode 'emacs)
;; (evil-set-initial-state 'magit-mode 'emacs)
;; ;; save company mode from vim bindings
;; (evil-define-key 'insert 'global (kbd "C-n") 'company-select-next)
;; (evil-define-key 'replace 'global (kbd "C-n") 'company-select-next)
;; (evil-define-key 'insert 'global (kbd "C-p") 'company-select-previous)
;; (evil-define-key 'replace 'global (kbd "C-p") 'company-select-previous)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
