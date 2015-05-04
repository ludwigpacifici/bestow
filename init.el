;;;;;;;;;;;;;;
;; packages ;;
;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(defvar my-packages '(
                      expand-region
                      ido-ubiquitous
                      markdown-mode
                      org
                      org-plus-contrib
                      scss-mode
                      solarized-theme
                      yaml-mode
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;;;;;
;; behavior ;;
;;;;;;;;;;;;;;
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(load-theme 'zenburn t)
(set-face-attribute 'default nil :family "Monaco")
(set-face-attribute 'default nil :height 145)
(global-hl-line-mode t) ;; Highlight current line
(setq x-stretch-cursor t) ;; Wide cursor on tabs
(setq ring-bell-function 'ignore) ;; Turn off alarms
(line-number-mode t)
(column-number-mode t)
(setq-default cursor-type 'box)
(blink-cursor-mode t)
(show-paren-mode t)
(setq initial-scratch-message "")
(setq inhibit-splash-screen t) ;; No splash screen
(setq frame-title-format "%b - Emacs") ;; Buffer name in the title bar
(setq icon-title-format "%b - Emacs") ;; Buffer name in the title bar
(delete-selection-mode t) ;; Delete selected text
(setq mode-require-final-newline t)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq auto-save-timeout 60) ;; Autosave every minute
(savehist-mode 1) ;; Save minibuffer historic
(setq user-mail-address "ludwig@lud.cc")
(toggle-frame-fullscreen)

;;;;;;;;;;;;;;;;;;
;; coding style ;;
;;;;;;;;;;;;;;;;;;
(setq require-final-newline 't) ;; newline at the end of the file
(setq-default indent-tabs-mode nil) ;; no tabs
(setq tab-width 4) ;; tabs size

;;;;;;;;;;;;;;;;;;;;;;;;
;; Character encoding ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-input-method nil) ;; No funky input for normal editing
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8") ;; prefer utf-8 for language settings
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward) ;; Overrides Emacs’ default mechanism for making buffer names unique

(require 'ido)
(ido-mode t)
(ido-everywhere t)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

(require 'saveplace)
(setq-default save-place t)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(require 'expand-region)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;;;;;;;;;;;;;;;
;; shortcuts ;;
;;;;;;;;;;;;;;;
(global-set-key [f11] 'toggle-frame-fullscreen)
(global-set-key "\M- " 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(windmove-default-keybindings 'meta) ;; Move point from window to window

;;;;;;;;;;;;;;
;; Mac OS X ;;
;;;;;;;;;;;;;;
;; key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-command-modifier 'meta))

;;;;;;;;;;;;;;
;; includes ;;
;;;;;;;;;;;;;;
(load-file "~/.emacs.d/groundless-fear.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a041a61c0387c57bb65150f002862ebcfe41135a3e3425268de24200b82d6ec9" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
