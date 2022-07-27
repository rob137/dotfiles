;; hide ui
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; follow simlinks back to original file
;; this helps make version control work better and prevents warnings about
;; opening simlink files
(setq-default vc-follow-symlinks t)

;; show number of search matches when using C-s / C-r
(setq-default isearch-lazy-count t)

;; navigate window layout history with C-c left / right
(winner-mode t)

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

;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(load-theme 'nord t)

(set-face-background 'trailing-whitespace "white")
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; for node, which is needed for prettier
(add-to-list 'exec-path "/usr/local/bin/node")

;; for autoformatting code with prettier
;; prettify highlighted text, or the whole file
(defun prettify ()
  (interactive)
  (if (region-active-p)
      (prettier-prettify-region)
    (prettier-prettify)))
(global-set-key (kbd "C-c p") 'prettify)

;; relative line numbers and bindings for toggling
;; would love a conditional to make this a single binding...
(setq-default display-line-numbers 'relative)
(global-set-key (kbd "C-c r n") 'menu-bar--display-line-numbers-mode-relative)
(global-set-key (kbd "C-c n r") 'menu-bar--display-line-numbers-mode-absolute)
;; show line / column numbers in mode line (at bottom)
(line-number-mode t)
(column-number-mode t)

;; show line numbers in left column when editing code
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Otherwise emacs only treats ".  "  as end of sentence
(setq-default sentence-end-double-space nil)

;; Delete selected text if you type while it is highlighted
(delete-selection-mode t)

;; automatically add delete closing parens
(electric-pair-mode t)

;; remember which files were last opened
;; use recentf-open-files to view them
(recentf-mode t)

;; show clock
 (display-time-mode 1)

;; hide fringe to hide ugly newline arrow icons on wrapped lines
;; Possibly not quite right - the fringe can show other things
;; e.g. "where a program you are debugging is executing"...
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Fringes.html
(fringe-mode '(0 . 0))
(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil nil) ;; no continuation indicators
      ;; '(nil right-curly-arrow) ;; right indicator only
      ;; '(left-curly-arrow nil) ;; left indicator only
      ;; '(left-curly-arrow right-curly-arrow) ;; default
      )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:update-interval 2)
 '(package-selected-packages
   '(projectile fzf counsel ag neotree git-gutter vimrc-mode ranger magit prettier nord-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; for mac os x https://www.emacswiki.org/emacs/FullScreen#h5o-27
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
(global-set-key (kbd "C-c C-f") 'toggle-fullscreen)
