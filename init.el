;;; init.el --- Ludwig's Emacs configuration

;;; Code:

;; Garbage collector kicks in when 100Mb is used
(setq gc-cons-threshold 50000000)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(setq user-mail-address "ludwig@lud.cc"
      user-full-name "Ludwig PACIFICI")

(use-package ggtags
  :ensure t
  :bind
  ("M-," . pop-tag-mark))

(use-package cc-mode
  :ensure t
  :config
  (add-hook 'c-mode-hook #'ggtags-mode)
  (add-hook 'c++-mode-hook #'ggtags-mode)
  (setq-default c-basic-offset 2)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package clang-format
  :ensure t
  :bind ("C-M-<tab>" . clang-format-region))

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cmake-font-lock
  :ensure t
  :config
  (add-hook 'cmake-mode-hook #'cmake-font-lock-activate))

(use-package d-mode
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package ido
  :ensure t
  :config
  (setq ido-auto-merge-work-directories-length -1
        ido-create-new-buffer 'always
        ido-default-file-method 'selected-window
        ido-enable-flex-matching t
        ido-use-filename-at-point nil)
  (ido-mode t)
  (ido-everywhere t))

(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous-mode +1))

(use-package ido-grid-mode
  :ensure t
  :config
  (setq ido-grid-mode-first-line nil
        ido-grid-mode-max-rows 3
        ido-grid-mode-min-rows 3)
  (ido-grid-mode +1))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode +1)
  ;; disable ido faces to see flx highlights
  (setq ido-use-faces nil))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package markdown-mode
  :ensure t)

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package saveplace
  :config
  (setq-default save-place t))

(use-package scss-mode
  :ensure t)

(use-package smex
  :ensure t
  :bind ("M-x" . smex))

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))

(use-package typit
  :ensure t)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package yaml-mode
  :ensure t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(blink-cursor-mode -1)
(column-number-mode t)
(delete-selection-mode t)
(delete-selection-mode t) ;; Delete selected text
(global-auto-revert-mode t) ;; Auto refresh buffers when edits occur outside emacs
(global-hl-line-mode t)
(line-number-mode t)
(menu-bar-mode 0)
(savehist-mode t) ;; Save minibuffer historic
(scroll-bar-mode -1)
(setq auto-save-timeout 60) ;; Autosave every minute
(setq current-language-environment "English")
(setq indent-tabs-mode nil)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq load-prefer-newer t)
(setq mode-require-final-newline t)
(setq require-final-newline 't) ;; newline at the end of the file
(setq ring-bell-function 'ignore) ;; Turn off alarms
(setq scroll-preserve-screen-position t)
(setq tab-width 2) ;; tabs size
(setq tramp-default-method "ssh")
(setq x-stretch-cursor t) ;; Wide cursor on tabs
(setq-default cursor-type 'box)
(setq-default indent-tabs-mode nil) ;; no tabs
(show-paren-mode t)
(size-indication-mode 0)
(toggle-frame-fullscreen)
(tool-bar-mode 0)

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

(defun dont-kill-emacs ()
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))

;;;;;;;;;;;;;;;
;; shortcuts ;;
;;;;;;;;;;;;;;;
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-x\C-c" 'dont-kill-emacs)
(global-set-key "\M- " 'hippie-expand)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key [f11] 'toggle-frame-fullscreen)
(global-unset-key (kbd "C-z"))
(windmove-default-keybindings 'meta) ;; Move point from window to window
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))
