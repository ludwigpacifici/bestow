;;; init.el --- Ludwig's Emacs configuration

;;; Code:

;; Garbage collector kicks later
(setq gc-cons-threshold (* 100 1000 1000))

;; Increase the amount of data which Emacs reads from the
;; process. Again the emacs default is too low 4k considering that the
;; some of the language server responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024 16)) ;; 16mb

(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(setq user-mail-address "ludwig@lud.cc"
      user-full-name "Ludwig PACIFICI")

(use-package exec-path-from-shell
  :ensure t
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "LD_LIBRARY_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package eglot
  :ensure t
  :preface
  (defun bestow/eglot-organize-imports ()
    (interactive)
    (if (and (eglot-managed-p)
             (eglot--server-capable :OrganizeImports))
        (eglot-code-actions nil nil "source.organizeImports" t)))
  (defun bestow/eglot-format-on-save ()
    (interactive)
    (if (eglot-managed-p)
        (eglot-format-buffer)))
  :hook (((rust-ts-mode c++-ts-mode python-mode) . eglot-ensure)
         (before-save . bestow/eglot-organize-imports)
         (before-save . bestow/eglot-format-on-save))
  :bind (:map eglot-mode-map
              ("s-l a" . eglot-code-actions)
              ("s-l r" . eglot-rename)
              ("s-l t" . eglot-find-typeDefinition))
  :config
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider)
        eglot-extend-to-xref t))

(use-package doc-view
  :config
  (setq doc-view-continuous t)
  (setq doc-view-resolution 500))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (vertico-indexed-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :bind (
         ("C-c e" . consult-compile-error)
         ("C-c f" . consult-flymake)
         ("C-c g" . consult-git-grep)
         ("C-c l" . consult-locate)
         ("C-c s" . consult-grep)
         ("C-x b" . consult-buffer)
         ("M-g M-g" . consult-goto-line)
         ("M-g g" . consult-goto-line)
         ("M-y" . consult-yank-pop)))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package corfu
  :ensure t
  :bind (:map corfu-map
              ("<tab>" . corfu-complete)
              ("C-M-i" . corfu-complete)
              ("TAB" . corfu-complete))
  :init (global-corfu-mode))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

(use-package avy
  :ensure t
  :config
  (setq-default avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o ?h))
  :bind
  ("C-;" . avy-goto-char-timer))

(use-package ace-window
  :ensure t
  :bind
  ("M-o" . ace-window))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :init
  (setq magit-repository-directories '(("~" . 1))
        magit-section-visibility-indicator nil))

(use-package markdown-mode
  :ensure t
  :init
  (setq markdown-command "/usr/bin/pandoc"))

(use-package cc-mode
  :ensure nil
  :init (setq-default c-basic-offset 4))

(use-package c++-ts-mode
  :defer t
  :mode "\\.cpp\\'")

(use-package rust-mode
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t
        rust-format-on-save t
        rust-format-show-buffer nil
        rust-format-goto-problem nil))

(use-package rust-ts-mode
  :ensure t
  :bind (("C-c C-d" . rust-dbg-wrap-or-unwrap)))


(use-package cargo
  :ensure t
  :hook (rust-ts-mode . cargo-minor-mode)
  :config
  (setq compilation-scroll-output t
        compilation-ask-about-save nil))

(use-package saveplace
  :config
  (setq-default save-place t))

(use-package minions
  :ensure t
  :config (minions-mode 1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package json-mode :ensure t)

(use-package lua-mode :ensure t)

(use-package toml-mode :ensure t)

(use-package yaml-mode :ensure t)

(use-package fish-mode :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package flymake
  :ensure t
  :config
  (setq flymake-fringe-indicator-position nil))

(use-package elpy
  :ensure t
  :init
  (setq elpy-formatter 'black)
  (add-hook 'elpy-mode-hook (lambda ()
                            (add-hook 'before-save-hook
                                      'elpy-black-fix-code nil t)))
  (elpy-enable))

(use-package modus-themes
  :ensure t
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-subtle-line-numbers t
        modus-themes-success-deuteranopia t
        modus-themes-fringes nil
        modus-themes-paren-match '(bold intense)
        modus-themes-diffs 'deuteranopia)
  (load-theme 'modus-vivendi :no-confim)
  :config
  (set-face-foreground 'vertical-border (face-background 'default))
  :bind ("<f5>" . modus-themes-toggle))

(use-package denote
  :ensure t
  :config
  (setq denote-directory (expand-file-name "~/notes-pour-trop-tard")
        denote-infer-keywords t
        denote-sort-keywords t))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(blink-cursor-mode -1)
(column-number-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(global-hl-line-mode 1)
(global-subword-mode)
(line-number-mode 1)
(menu-bar-mode 0)
(savehist-mode 1)
(recentf-mode 1)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :family "Iosevka Term" :height 150 :weight 'normal :width 'normal :slant 'normal)
(setq indent-tabs-mode nil
      tab-width 4
      tab-always-indent 'complete)

(setq-default auto-save-timeout 60
              comp-async-report-warnings-errors nil
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
              shift-select-mode nil
              tramp-default-method "ssh"
              use-dialog-box nil
              x-stretch-cursor t
              enable-recursive-minibuffers t)

(show-paren-mode t)
(size-indication-mode 1)
(tool-bar-mode 0)
(transient-mark-mode t)
(setq frame-resize-pixelwise t)

(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(remove-hook 'xref-after-return-hook 'xref-pulse-momentarily)
(remove-hook 'xref-after-jump-hook 'xref-pulse-momentarily)

(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")))

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

(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-unset-key (kbd "C-z"))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))
(setq x-super-keysym nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
(message "%s" (emacs-init-time))

;; init.el ends here
