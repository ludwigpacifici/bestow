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

(use-package xah-lookup
  :ensure t)

(use-package cc-mode
  :ensure t
  :config
  (add-hook 'c-mode-hook #'ggtags-mode)
  (add-hook 'c++-mode-hook #'ggtags-mode)
  (setq-default c-basic-offset 2)
  (setq c-default-style "linux")
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (defun xah-lookup-cppreference (&optional word)
    "Lookup definition of current word or text selection in URL."
    (interactive)
    (xah-lookup-word-on-internet
     word
     "http://en.cppreference.com/mwiki/index.php?search=�"
     xah-lookup-browser-function))
  (define-key c++-mode-map (kbd "C-c d") #'xah-lookup-cppreference)
  (defun xah-lookup-boost (&optional word)
    (interactive)
    (xah-lookup-word-on-internet
     word
     "https://cse.google.com/cse?cx=011577717147771266991:jigzgqluebe&q=�"
     xah-lookup-browser-function))
  (define-key c++-mode-map (kbd "C-c b") #'xah-lookup-boost)
  (define-key c++-mode-map (kbd "C-M-<tab>") #'clang-format-region))

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
  :ensure t)

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

(use-package doc-view
  :config
  (setq doc-view-continuous t)
  (setq doc-view-resolution 500))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package faceup
  :ensure t)

(use-package font-lock-studio
  :ensure t)

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

(use-package json-mode
  :ensure t)

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
  :ensure t
  :config
  (setq markdown-command "/sbin/pandoc"))

(use-package octave-mode
  :mode "\\.m\\'"  :config
  (add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
  (add-hook 'octave-mode-hook
            (lambda ()
              (abbrev-mode 1)
              (auto-fill-mode 1)
              (if (eq window-system 'x)
                  (font-lock-mode 1)))))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package rust-mode
  :ensure t)

(use-package saveplace
  :config
  (setq-default save-place t))

(use-package scss-mode
  :ensure t)

(use-package smex
  :ensure t
  :bind ("M-x" . smex))

(use-package spaceline-config
  :ensure spaceline
  :config
  (setq-default powerline-default-separator 'bar
                powerline-height 35
                spaceline-highlight-face-func 'spaceline-highlight-face-modified)
  (spaceline-emacs-theme))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package yaml-mode
  :ensure t)

(use-package ibuffer-vc
  :ensure t
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(blink-cursor-mode -1)
(column-number-mode t)
(delete-selection-mode t)
(delete-selection-mode t)
(global-auto-revert-mode t)
(global-hl-line-mode t)
(line-number-mode t)
(menu-bar-mode 0)
(savehist-mode t)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :family "Meslo LG M Regular for Powerline" :height 100)
(setq indent-tabs-mode nil
      tab-width 2)
(setq-default auto-save-timeout 60
              current-language-environment "English"
              cursor-in-non-selected-windows t
              cursor-type 'box
              grep-highlight-matches t
              grep-scroll-output t
              indent-tabs-mode nil
              inhibit-splash-screen t
              initial-scratch-message ""
              load-prefer-newer t
              mode-require-final-newline t
              require-final-newline 't
              ring-bell-function 'ignore
              scroll-preserve-screen-position t
              tramp-default-method "ssh"
              x-stretch-cursor t)
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
(global-set-key "\C-x\C-c" 'dont-kill-emacs)

(defun dont-suspend-emacs ()
  (interactive)
  (error (substitute-command-keys "To suspend emacs: \\[suspend-frame]")))
(global-set-key "\C-x\C-z" 'dont-suspend-emacs)

(defun endless/goto-match-beginning ()
  "Go to the start of current isearch match.
Use in `isearch-mode-end-hook'."
  (when (and isearch-forward
             (number-or-marker-p isearch-other-end)
             (not mark-active)
             (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))
(add-hook 'isearch-mode-end-hook #'endless/goto-match-beginning)

(defun endless/isearch-symbol-with-prefix (p)
  "Like isearch, unless prefix argument is provided.
With a prefix argument P, isearch for the symbol at point."
  (interactive "P")
  (let ((current-prefix-arg nil))
    (call-interactively
     (if p #'isearch-forward-symbol-at-point
       #'isearch-forward))))
(global-set-key [remap isearch-forward] #'endless/isearch-symbol-with-prefix)

(global-set-key "\C-cc" 'compile)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key [f11] 'toggle-frame-fullscreen)
(global-unset-key (kbd "C-z"))
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))
(setq x-super-keysym 'meta)
