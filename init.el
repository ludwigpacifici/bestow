;;; init.el --- Ludwig's Emacs configuration

;;; Code:

;; Garbage collector kicks in when 100Mb is used
(setq gc-cons-threshold 50000000)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(setq user-mail-address "ludwig@lud.cc"
      user-full-name "Ludwig PACIFICI")

(use-package exec-path-from-shell
  :ensure t
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package parinfer
  :ensure t
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             pretty-parens  ; different paren styles for different modes.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

(use-package company
  :ensure t)

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
  (setq c-default-style "linux")
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (define-key c++-mode-map (kbd "C-M-<tab>") #'clang-format-region))

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'parinfer-mode))

(use-package clang-format
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'company-mode)
  (add-hook 'clojure-mode-hook #'parinfer-mode)
  (add-hook 'clojure-mode-hook #'subword-mode))

(use-package inf-clojure
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode))

(use-package doc-view
  :config
  (setq doc-view-continuous t)
  (setq doc-view-resolution 500))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package faceup
  :ensure t)

(use-package font-lock-studio
  :ensure t)

(use-package smex
  :ensure t)

(use-package avy
  :ensure t
  :bind
  ("C-'" . avy-goto-char-2))

(use-package counsel
  :ensure t
  :config
  (setq-default counsel-find-file-ignore-regexp "~$"))

(use-package swiper
  :ensure t
  :bind ("C-c i s" . swiper))

(use-package ivy
  :ensure t
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-c i l" . counsel-locate)
  ("C-c i r" . counsel-rg)
  ("C-c i g" . counsel-git-grep)
  :config
  (ivy-mode 1)
  (setq-default enable-recursive-minibuffers t
                ivy-count-format ""
                ivy-display-style nil
                ivy-format-function (quote ivy-format-function-arrow)
                ivy-initial-inputs-alist nil
                ivy-re-builders-alist '((t . ivy--regex-fuzzy))
                ivy-use-virtual-buffers t)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package json-mode
  :ensure t)

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'company-mode)
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

(use-package rust-mode
  :ensure t
  :config
  (setq racer-rust-src-path "~/.multirust/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
  (add-hook 'rust-mode-hook #'rust-enable-format-on-save)
  (add-hook 'rust-mode-hook #'racer-mode))

(use-package racer
  :ensure t
  :bind
  ("TAB" . company-indent-or-complete-common)
  :config
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (setq company-tooltip-align-annotations t))

(use-package cargo
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package saveplace
  :config
  (setq-default save-place t))

(use-package scss-mode
  :ensure t)

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t)
  (set-face-foreground 'vertical-border (face-background 'default)))

;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (load-theme 'sanityinc-tomorrow-day t)
;;   (set-face-foreground 'vertical-border (face-background 'default)))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package toml-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

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

(use-package js2-mode
  :ensure t
  :config
  (setq-default js2-basic-offset 2
                js2-bounce-indent-p nil
                js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode)))

(use-package prettier-js
  :ensure t
  :config
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'js2-mode-hook 'prettier-js-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(blink-cursor-mode -1)
(column-number-mode 0)
(delete-selection-mode t)
(global-auto-revert-mode t)
(global-hl-line-mode t)
(line-number-mode 0)
(menu-bar-mode 0)
(savehist-mode t)
(scroll-bar-mode -1)
(set-cursor-color "#d54e53")
(set-face-attribute 'default nil :family "Fira Code Medium" :height 158)
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
              mode-line-format nil
              mode-require-final-newline t
              require-final-newline 't
              ring-bell-function 'ignore
              scroll-preserve-screen-position t
              tramp-default-method "ssh"
              x-stretch-cursor t)
(show-paren-mode t)
(size-indication-mode 0)
(tool-bar-mode 0)
(transient-mark-mode t)
(toggle-frame-fullscreen)

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

(defun endless/isearch-symbol-with-prefix (p)
  "Like isearch, unless prefix argument is provided.
With a prefix argument P, isearch for the symbol at point."
  (interactive "P")
  (let ((current-prefix-arg nil))
    (call-interactively
     (if p #'isearch-forward-symbol-at-point
       #'isearch-forward))))
(global-set-key [remap isearch-forward] #'endless/isearch-symbol-with-prefix)

(defun lud/show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer-name (buffer-name)))
    (if filename
        (message "%s" filename)
      (message "%s" buffer-name))))
(global-set-key (kbd "C-c m f") 'lud/show-file-name)

(defun lud/show-current-time ()
  "Show the full time in the minibuffer."
  (interactive)
  (message "%s" (current-time-string)))
(global-set-key (kbd "C-c m t") 'lud/show-current-time)

(defun lud/show-modified-buffer ()
  "Show state of the current in the minibuffer."
  (interactive)
  (cond
   (buffer-read-only (message "%s" "Read only"))
   ((buffer-modified-p) (message "%s" "Modified"))
   (t (message "%s" "Already saved"))))
(global-set-key (kbd "C-c m s") 'lud/show-modified-buffer)

(defun lud/current-git-branch ()
  "Show the current Git branch in the minibuffer."
  (interactive)
  (let ((branch (car (vc-git-branches))))
    (if branch
        (message "%s" "On branch " branch)
      (message "%s" "Not a Git branch"))))
(global-set-key (kbd "C-c m b") 'lud/current-git-branch)

(defun lud/current-encoding ()
  "Show the current encoding in the minibuffer."
  (interactive)
  (message "%s" buffer-file-coding-system))
(global-set-key (kbd "C-c m e") 'lud/current-encoding)

(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c l") 'what-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cr" 'recompile)
(global-set-key "\C-cg" 'rgrep)
(global-set-key [f11] 'toggle-frame-fullscreen)
(global-unset-key (kbd "C-z"))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))
(setq x-super-keysym 'meta)

;; init.el ends here
