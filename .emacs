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
;; directory - e.g. when npm is installed via nvm (and t always should be)
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
   '(gotest dtrt-indent highlight cov coverage indium restclient which-key docker dockerfile-mode doom-themes go-mode jenkinsfile-mode evil-collection evil expand-region hl-todo flycheck eslint-rc prettier-rc scss-mode exec-path-from-shell web-mode docker-compose-mode lsp-mode rg rjsx-mode typescript-mode tree-sitter-langs tree-sitter projectile fzf counsel ag neotree git-gutter vimrc-mode ranger magit prettier))
 '(safe-local-variable-values
   '((cov-lcov-file-name . "/Users/robert.kirby/g/events-service/coverage/lcov.info")
     (lcov-file-name . "/Users/robert.kirby/g/events-service/coverage/lcov.info"))))









































;; ============= EMACS NATIVE SETTINGS =============

;; hide ui
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; (set-frame-parameter (selected-frame) 'alpha '(90 . 0)) ;; Make emacs windows transparent

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

;; partial matching when entering buffer, file, directory names
;; disable for given input with C-b for buffers, C-b for files
(setq-default ido-enable-flex-matching t)
(setq-default ido-everywhere t)
(ido-mode t)

;; Don't display the 'Welcome to GNU Emacs' buffer on startup
(setq-default inhibit-startup-message t)

;; instead of icomplete - covers M-x and help too
;; http://xahpmlee.info/emacs/emacs/emacs_icomplete_mode.html
(fido-mode t)

;; ;; show tabs, spaces, newlines etc
(global-whitespace-mode t)
;; All of the defaults, minus line / lines-tail newline / newline mark, which were too
;; noisy; added line-tail to highlight end of long lines
(setq-default whitespace-style '(face
                                 spaces
                                 trailing
                                 space-before-tab
                                 indentation
                                 empty
                                 space-after-tab
                                 space-mark
                                 tab
                                 ;; tab-mark ;; ugly, can't seem to change color
                                 missing-newline-at-eof))


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
(global-set-key (kbd "C-c t") 'term-zsh)

;; org mode fully expand tree under cursor - s-Tab does globally, not always what I want
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c e") 'org-show-subtree))

;; org mode add a day as header to next line, then move point under it, for logging activity
(fset 'org-new-day
      (kmacro-lambda-form [?\C-e return ?* ?* ?* ?  ?\C-c ?. ?  return return] 0 "%d"))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c a d") 'org-new-day))

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
(global-set-key (kbd "C-c C-f") 'toggle-fullscreen)

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
    (if (string-match "[a-zA-Z]+-[0-9]\\{1,\\}" branch)
        (insert (upcase (match-string 0 branch)) ": ")
      )))
;; On opening magit commit message, insert ticket number
(add-hook 'git-commit-setup-hook 'insert-ticket-number)

(global-set-key (kbd "C-=") 'er/expand-region)


















;; ============= PACKAGES =============

;; outsource autoindentation to dtrt-indent, as it's a pain
(dtrt-indent-global-mode t)

;; enable autocompletion
(global-company-mode)
;; but disable for modes
(setq company-global-modes '(not-eshell-mode))
(setq company-global-modes '(not-shell-mode))
(setq company-global-modes '(not-term-mode))

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

;; some ease of use bindings for code familiar from my vimrc
(global-set-key (kbd "C-c d") 'lsp-find-definition)
(global-set-key (kbd "C-c f") 'ffap)

;; set up the lsp server
(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'js-jsx-mode-hook #'lsp)
(add-hook 'html-mode-hook #'lsp)
(add-hook 'css-mode-hook #'lsp)


(setq lsp-enable-symbol-highlighting t)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-doc-show-with-mouse t)

;; Ever-growing list of mappings of filetype to language mode.
;; Note that these will override any defaults, since they'll be appended to the
;; auto-mode-list and therfore read last
(setq auto-mode-alist
      (append '(  ; note these are encapsulated in a '() list
                ("\\.js\\'" . typescript-mode)
                ("\\.jsx\\'" . typescript-mode)
                ("\\.ts\\'" . typescript-mode)
                ("\\.tsx\\'" . typescript-mode)

                ;; Wrap words, truncate lines
                ("\\.txt\\'" . visual-line-mode)

                ("\\.go\\'" . go-mode)
                )
              auto-mode-alist))

;; Color theme
(require 'doom-themes)
(load-theme 'doom-nord t)
;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

(fset 'next-theme
      (kmacro-lambda-form [?\C-x ?r ?b ?. ?e ?m ?a tab return ?\M-< ?\C-s ?d ?o ?o ?m ?- ?\C-s ?\C-s ?\M-d ?\C-/ ?\M-x ?z ?p backspace ?a ?p ?- ?u ?p ?- ?t ?o ?- ?c ?h ?h ?a ?r backspace backspace backspace ?a ?r return ?  ?\C-x ?\C-f ?~ ?/ ?s ?a ?n ?d ?p ?i ?t ?/ ?t ?h ?e ?m ?e ?s ?. ?t ?x ?t return ?\M-< ?\C-s ?\C-y return ?\C-n ?\C-a ?\C-s ?- return ?\C-k ?\C-/ ?\C-x ?b return ?\C-y ?\C-e ?\C-x ?\C-e] 0 "%d"))

(global-set-key (kbd "C-c n") 'next-theme)

;; Use a different color for the active window's modeline
(set-face-background 'mode-line "#3B4252")


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


;; golang
;; On entering go mode, start lsp
(add-hook 'go-mode-hook #'lsp-deferred)
;; Shortcuts for common go-test invocations.
(add-hook 'go-mode-hook (lambda ()
  (company-mode) ; enable company upon activating go

  ;; Code layout.
  (setq tab-width 2 indent-tabs-mode 1) ; std go whitespace configuration
  (add-hook 'before-save-hook 'gofmt-before-save) ; run gofmt on each save

  ;; Shortcuts for common go-test invocations.
  (let ((map go-mode-map))
    (define-key map (kbd "C-c g t p") 'go-test-current-project) ;; current package, really
    (define-key map (kbd "C-c g t f") 'go-test-current-file)
    (define-key map (kbd "C-c g t t") 'go-test-current-test)
    )
  ))


















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
