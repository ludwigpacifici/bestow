;;; init.el --- Ludwig's Emacs configuration

;;; Code:

;; Garbage collector kicks later
(setq gc-cons-threshold (* 100 1000 1000))

;; Increase the amount of data which Emacs reads from the
;; process. Again the emacs default is too low 4k considering that the
;; some of the language server responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024 16)) ;; 16mb

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))

(setq user-mail-address "ludwig@lud.cc"
      user-full-name "Ludwig PACIFICI")

(use-package exec-path-from-shell
  :ensure t
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "LD_LIBRARY_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package emacs
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package savehist
  :init
  (savehist-mode))

(use-package doc-view
  :config
  (setq doc-view-continuous t)
  (setq doc-view-resolution 500))

(use-package expand-region
  :ensure t
  :bind ("M-SPC" . er/expand-region))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (vertico-indexed-mode))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

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
         ("C-c f" . consult-find)
         ("C-c g" . consult-git-grep)
         ("C-c s" . consult-ripgrep)
         ("C-c o" . consult-outline)
         ("M-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-g M-g" . consult-goto-line)
         ("M-g g" . consult-goto-line)
         ("M-y" . consult-yank-pop)))

(use-package company
  :after lsp-mode
  :ensure t
  :config
  (setq company-format-margin-function nil
        company-idle-delay 16)
  :bind
  (:map company-mode-map
        ("C-M-i". company-indent-or-complete-common)
        ("<tab>". company-indent-or-complete-common)
        ("TAB". company-indent-or-complete-common))
  :hook (after-init . global-company-mode))

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
  ("C-;" . avy-goto-char-2))

(use-package ace-window
  :ensure t
  :config
  (setq-default aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o ?h))
  :bind
  ("M-O" . ace-window))

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
        magit-section-visibility-indicators nil
        magit-save-repository-buffers 'dontask))

(use-package markdown-mode
  :ensure t
  :init
  (setq markdown-command "/usr/bin/pandoc"))

(use-package typst-ts-mode
  :ensure t)

(use-package cc-mode
  :ensure nil
  :init (setq-default c-basic-offset 4))

(use-package yasnippet-snippets
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (yas-global-mode t)
  :hook (after-init . yas-minor-mode))

(use-package rust-mode
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t
        rust-format-on-save t
        rust-format-show-buffer nil
        rust-format-goto-problem nil))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode)
  :config
  (setq compilation-scroll-output nil
        compilation-ask-about-save nil))

(use-package clang-format
  :ensure t
  :config
  (defun clang-format-save-hook-for-this-buffer ()
    "Create a buffer local save hook."
    (add-hook 'before-save-hook
              (lambda ()
                (when (locate-dominating-file "." ".clang-format")
                  (clang-format-buffer))
                ;; Continue to save.
                nil)
              nil
              ;; Buffer local hook.
              t))
  (add-hook 'c-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
  (add-hook 'c++-mode-hook (lambda () (clang-format-save-hook-for-this-buffer))))

(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq dired-listing-switches "-alFh"
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-dwim-target t
        delete-by-moving-to-trash t))

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

(use-package elfeed
  :ensure t
  :bind ("C-c w" . elfeed)
  :config
  (setq-default elfeed-search-filter "@6-months-ago"))

(use-package modus-themes
  :ensure t
  :config
  (setq modsu-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia))
  (load-theme 'modus-vivendi-deuteranopia :no-confirm)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(use-package denote
  :ensure t
  :config
  (setq denote-directory (expand-file-name "~/notes-pour-trop-tard")
        denote-infer-keywords t
        denote-sort-keywords t))

(use-package rg
  :ensure t
  :bind ("C-c r" . rg))

(use-package lsp-mode
  :ensure t
  :hook ((rust-mode . lsp)
         (c++-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-inlay-hints-mode))
  :commands lsp
  :bind (("C-c d" . lsp-rust-analyzer-open-external-docs))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq lsp-enable-symbol-highlighting nil
        lsp-headerline-breadcrumb-enable nil
        lsp-inlay-hint-enable t
        lsp-lens-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-signature-render-documentation nil
        lsp-diagnostics-provider :none))

(use-package jinx :ensure t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(blink-cursor-mode -1)
(column-number-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(setq global-hl-line-sticky-flag nil)
(global-hl-line-mode 1)
(global-subword-mode)
(line-number-mode 1)
(recentf-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(let ((mono-spaced-font "Iosevka Fixed Slab")
      (proportionately-spaced-font "Atkinson Hyperlegible"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 150 :weight 'medium :width 'normal :slant 'normal)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

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
              load-prefer-newer t
              mode-require-final-newline t
              require-final-newline 't
              scroll-preserve-screen-position t
              shift-select-mode nil
              tramp-default-method "ssh"
              use-dialog-box nil
              x-stretch-cursor t)


(show-paren-mode t)
(size-indication-mode 1)

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
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (typst "https://github.com/uben0/tree-sitter-typst")))

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
(global-set-key (kbd "M-o") 'other-window)
(global-unset-key (kbd "C-z"))

(setq mac-command-modifier 'control)
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
