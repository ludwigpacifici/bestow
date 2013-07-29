(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defvar my-packages '(clojure-mode
                      clojure-test-mode
                      
                      nrepl
                      paredit
                      solarized-theme))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; coding look
(load-theme 'solarized-dark t)
(set-face-attribute 'default nil :font "Monaco" :height 113)
(global-hl-line-mode t)
(setq line-number-mode t)
(setq column-number-mode t)
(show-paren-mode t)
(set-cursor-color "red")

;; gui
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)

;; behavior
(windmove-default-keybindings 'meta) ;; easy switching between visible buffers
(ido-mode t)
(iswitchb-mode t)
(savehist-mode 1) ;; save minibuffer historic
(setq frame-title-format "%b - Emacs") ;; buffer name in the title bar
(setq icon-title-format "%b - Emacs") ;; buffer name in the title bar
(fset 'yes-or-no-p 'y-or-n-p) ;; yes-no shortcuts
(delete-selection-mode t) ;; delete selected text
(setq x-stretch-cursor t) ;; cursor as wide as the character it is over

;; coding style
(setq require-final-newline 't) ;; newline at the end of the file
(setq-default indent-tabs-mode nil) ;; no tabs

;; Character encoding:
;; utf-8 / input-method
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8") ;; prefer utf-8 for language settings
(set-input-method nil) ;; no funky input for normal editing
