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
   '(doom-themes go-mode jenkinsfile-mode evil-collection evil expand-region hl-todo flycheck eslint-rc prettier-rc scss-mode exec-path-from-shell web-mode docker-compose-mode lsp-mode rg rjsx-mode typescript-mode tree-sitter-langs tree-sitter projectile fzf counsel ag neotree git-gutter vimrc-mode ranger magit prettier)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )













;; ============= EMACS NATIVE SETTINGS =============

;; hide ui
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


(set-frame-parameter (selected-frame) 'alpha '(95 . 95)) ;; Make emacs windows transparent

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

;; more matching - covers M-x and help too
;; C-j to run highlighted command if there's still multiple suggestions
;; RET to run if only one suggestion remains, i.e. "[matched]"
;; For differences, see:
;; http://xahlee.info/emacs/emacs/emacs_icomplete_vs_ido_27.html
(icomplete-mode t)

;; ;; show tabs, spaces, newlines etc
(global-whitespace-mode t)
;; All of the defaults, minus line / lines-tail newline / newline mark, which were too
;; noisy; added line-tail to highlight end of long lines
(setq-default whitespace-style '(face tab spaces trailing space-before-tab
                                      indentation empty space-after-tab
                                      space-mark tab-mark
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

(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; for node, which is needed for prettier
(add-to-list 'exec-path "/usr/local/bin/node")

;; To left of buffer
(setq-default display-line-numbers)
;; modeline
(line-number-mode t)
(column-number-mode t)

;; show line numbers in left column when editing code
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'markdown-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)

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

;;; Preserve contents of system clipboard
(setq save-interprogram-paste-before-kill t)

;; remember which files were last opened
;; use recentf-open-files to view them
(recentf-mode t)

;; show clock
(display-time-mode 1)
(setq display-time-24hr-format t)

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
;; snow errors, linting etc in left column
(fringe-mode)












;; ============= MAPPINGS / FUNCTIONS =============

;; In term mode, make the keys for setting char / line mode *toggle between*
;; char / line mode, saving headaches
;; Taken from: http://joelmccracken.github.io/entries/switching-between-term-mode-and-line-mode-in-emacs-term/
(require 'term)
(defun jnm/term-toggle-mode ()
  ;; "Toggles term between line mode and char mode"
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))
(define-key term-mode-map (kbd "C-c C-j") 'jnm/term-toggle-mode)
(define-key term-mode-map (kbd "C-c C-k") 'jnm/term-toggle-mode)
(define-key term-raw-map (kbd "C-c C-j") 'jnm/term-toggle-mode)

;; org mode fully expand tree under cursor - s-Tab does globally, not always what I want
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c e") 'org-show-subtree))

;; org mode add a day as header to next line, then move point under it, for logging activity
(fset 'org-add-day
      (kmacro-lambda-form [?\C-e return ?* ?* ?* ?  ?\C-c ?. ?  return return] 0 "%d"))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c a d") 'org-add-day))

;; Add a bunch of ffap-related bindings
(ffap-bindings) ;; is this working?
(setq ffap-require-prefix t)
;; Chiefly:
;; C-x C-f filename RET
;; Find filename, guessing a default from text around point (find-file-at-point).
;; C-x 4 f
;; ffap-other-window, analogous to find-file-other-window.
;; C-x d directory RET
;; Start Dired on directory, defaulting to the directory name at point (ffap-dired-at-point).

;; for autoformatting code with prettier
;; prettify highlighted text, or the whole file
(defun prettify ()
  (interactive)
  (if (region-active-p)
      (prettier-prettify-region)
    (prettier-prettify)))
;; on either condition, I want to append the follwing to the end:
;; exchange-point-and-mark
;; indent-for-tab-command
;; pop-to-mark-command
(global-set-key (kbd "C-c p") 'prettify) ;; probably shrould be a single binding for all formatters, which listens for correct file type...
(global-set-key (kbd "C-c c") 'compile)

;; for mac os x https://www.emacswiki.org/emacs/FullScreen#h5o-27
(defun toggle-fullscreen ()
  ;; "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
(global-set-key (kbd "C-c C-f") 'toggle-fullscreen)

;; backup / autosave / lock files - don't litter directories
(setq backup-directory-alist '((".emacs-backups")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs-saves/" t)))
(setq create-lockfiles nil)

(setq-default indent-tabs-mode nil)

;; For commit message, grab ticket number from branch name and insert at top of commit message
(fset 'add-ticket-number-as-title
      (kmacro-lambda-form [?\C-s ?A ?F ?F return ?\C-b ?\C-b ?\C-b ?\C-  ?\M-f ?\M-f ?\M-w ?\M-< ?\C-y ?: ? ] 0 "%d"))
(with-eval-after-load 'magit
  (define-key git-commit-mode-map (kbd "C-c t") 'add-ticket-number-as-title))








;; ============= DEPENDENCIES =============

;; copilot, needs the dependencies dash, s, editorconfig, at least it did when I installed it acording to its github README
(load-file "~/.emacs.d/copilot.el/dash.el")
(load-file "~/.emacs.d/copilot.el/copilot.el")
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key prog-mode-map (kbd "C-c c t") 'copilot-mode) ;; toggle copilot
(define-key prog-mode-map (kbd "C-c c c") 'copilot-mode) ;; alias for toggle
(define-key prog-mode-map (kbd "C-c c y") 'copilot-accept-completion) ;; yes
(define-key prog-mode-map (kbd "C-c c n") 'copilot-next-completion) ;; next
(define-key prog-mode-map (kbd "C-c c p") 'copilot-previous-completion) ;; prev

;; enable autocompletion
(global-company-mode)
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
      ;; to match nord theme - it seems like doom-themes' doom-nord has it covered
      ;; company-quickhelp-color-background "#3d4454"
      ;; company-quickhelp-color-foreground "#d8dee9"
      ;; To prevent completions always appearing in lowercase - annoying in most languages
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case "keep-prefix"
)

;; pop-up showing next possible key press - similar natively by '[keypress] ?'
;; (which-key-mode)
;; (which-key-setup-side-window-right)
;; (setq which-key-idle-delay 5.0)

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
;; (global-set-key (kbd "C-c g b") 'vc-annotate)  ;; removed to learn default: C-x v g
(global-set-key (kbd "C-c g d") 'magit-diff-buffer-file)
;; can probably get rid, as flycheck has C-c ! n/p for same
(global-set-key (kbd "C-c [ g") 'flycheck-previous-error)
(global-set-key (kbd "C-c ] g") 'flycheck-next-error)
(global-set-key (kbd "C-u") 'universal-argument)

;; TODO add loop function (DRY)
(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'js-jsx-mode-hook #'lsp)
(add-hook 'html-mode-hook #'lsp)
(add-hook 'css-mode-hook #'lsp)

(setq lsp-enable-symbol-highlighting t)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-doc-show-with-mouse t)
(setq lsp-ui-sideline-enable t)


;; hl TODO
(setq hl-todo-keyword-faces '(("TODO"   . "#ebcb8b")))

;; Ever-growing list of mappings of filetype to language mode.
;; Note that these will override any defaults, since they'll be appended to the
;; auto-mode-list and therfore read last
(setq auto-mode-alist
      (append '(  ; note these are encapsulated in a '() list
	        ("\\.js\\'" . typescript-mode)
	        ("\\.jsx\\'" . typescript-mode)
	        ("\\.ts\\'" . typescript-mode)
	        ("\\.tsx\\'" . typescript-mode)
	        ("\\.txt\\'" . visual-line-mode)
                )
              auto-mode-alist))

    (setq auto-mode-alist
          (append '(("\\.foo\\'" . foo-mode)  ; note these are encapsulated in a '() list
                    ("\\.bar\\'" . bar-mode))
                   auto-mode-alist))

(global-set-key (kbd "C-=") 'er/expand-region)

;; Theme
(require 'doom-themes)
(load-theme 'doom-nord t)
(setq-default doom-nord-brighter-comments 1)
(setq-default doom-nord-brighter-modeline 1)
;; Global settings (defaults)
;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)














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
