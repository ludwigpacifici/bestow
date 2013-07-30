;;;;;;;;;;;;;;
;; packages ;;
;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(setq url-proxy-services '(("no_proxy" . "work\\.com")
                           ("http" . "proxy:3128")))
(package-initialize)

(defvar my-packages '(auto-complete
                      clojure-mode
                      clojure-test-mode                      
                      nrepl
                      paredit
                      solarized-theme
                      yasnippet))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;;;;;;;;
;; coding look ;;
;;;;;;;;;;;;;;;;;
(load-theme 'solarized-dark t)
(set-face-attribute 'default nil :font "Monaco" :height 113)
(global-hl-line-mode t)
(setq line-number-mode t)
(setq column-number-mode t)
(show-paren-mode t)
(set-cursor-color "red")

;;;;;;;;;
;; gui ;;
;;;;;;;;;
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)

;;;;;;;;;;;;;;
;; behavior ;;
;;;;;;;;;;;;;;
(windmove-default-keybindings 'meta) ;; easy switching between visible buffers
(ido-mode t)
(iswitchb-mode t)
(savehist-mode 1) ;; save minibuffer historic
(setq frame-title-format "%b - Emacs") ;; buffer name in the title bar
(setq icon-title-format "%b - Emacs") ;; buffer name in the title bar
(fset 'yes-or-no-p 'y-or-n-p) ;; yes-no shortcuts
(delete-selection-mode t) ;; delete selected text
(setq x-stretch-cursor t) ;; cursor as wide as the character it is over
(electric-pair-mode t) ;; handle open/close brackets
(yas-global-mode t) ;; yasnippet
;; auto complete mod
;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;; set the trigger key so that it can work together with yasnippet on tab key,
;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;;;;;;;;;;;;;;;;;;
;; coding style ;;
;;;;;;;;;;;;;;;;;;
(setq require-final-newline 't) ;; newline at the end of the file
(setq-default indent-tabs-mode nil) ;; no tabs
(setq tab-width 4) ;; tabs size

;;;;;;;;;;;;;;;
;; C++ style ;;
;;;;;;;;;;;;;;;
(setq-default c-basic-offset 3) ;; indent size
(setq c-default-style "k&r") ;; indent style
(add-to-list 'auto-mode-alist '("\\.r\\'" . c-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Character encoding ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; utf-8 / input-method
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8") ;; prefer utf-8 for language settings
(set-input-method nil) ;; no funky input for normal editing
