(setq gc-cons-threshold 100000000)
(load-file "~/.emacs.d/init-packages.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac Os X environment variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))

;;;;;;;;;;;;;;
;; behavior ;;
;;;;;;;;;;;;;;
(setq x-stretch-cursor t) ;; Wide cursor on tabs
(setq ring-bell-function 'ignore) ;; Turn off alarms
(line-number-mode t)
(column-number-mode t)
(setq-default cursor-type 'box)
(blink-cursor-mode 0)
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
(setq current-language-environment "English")
(delete-selection-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-auto-revert-mode 1) ;; Auto refresh buffers when edits occur outside emacs
(show-paren-mode 1)
(setq scroll-preserve-screen-position 1)
(size-indication-mode 0)

(when (display-graphic-p)
  (load-theme 'solarized-dark t)
  (global-hl-line-mode t)
  (tool-bar-mode 0)
  (scroll-bar-mode -1))

(if (eq system-type 'darwin)
    (progn
      (set-face-attribute 'default nil :family "Monaco")
      (set-face-attribute 'default nil :height 115))
  (set-face-attribute 'default nil :family "Courier 10 Pitch")
  (set-face-attribute 'default nil :height 130))

(set-cursor-color "gold")
(menu-bar-mode 0)

;;;;;;;;;;;;;;;;;;
;; coding style ;;
;;;;;;;;;;;;;;;;;;
(setq require-final-newline 't) ;; newline at the end of the file
(setq-default indent-tabs-mode nil) ;; no tabs
(setq tab-width 2) ;; tabs size
(setq indent-tabs-mode nil)

;;;;;;;;;
;; C++ ;;
;;;;;;;;;
(require 'cc-mode)
(setq-default c-basic-offset 2)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;;;;;;;;;
;; Tags ;;
;;;;;;;;;;
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))


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
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;;;;;;;;;;;;;
;; Modules ;;
;;;;;;;;;;;;;
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-auto-merge-work-directories-length -1
      ido-enable-flex-matching t
      ido-use-faces nil)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)
(require 'smex)
(smex-initialize)
(require 'flx-ido)
(flx-ido-mode 1)
(ido-grid-mode 1)
(setq ido-grid-mode-first-line nil
      ido-grid-mode-max-rows 3
      ido-grid-mode-min-rows 3)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward) ;; Overrides Emacs’ default mechanism for making buffer names unique

(require 'saveplace)
(setq-default save-place t)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(require 'expand-region)

(autoload 'markdown-mode "markdown-mode" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

(setq tramp-default-method "ssh")

(defun dont-kill-emacs ()
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(add-hook 'prog-mode-hook #'rainbow-mode)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)

;;;;;;;;;;;;;;;
;; shortcuts ;;
;;;;;;;;;;;;;;;
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-x\C-c" 'dont-kill-emacs)
(global-set-key "\M- " 'hippie-expand)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "M-x") 'smex)
(global-set-key [C-M-tab] 'clang-format-region)
(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
(global-set-key [f11] 'toggle-frame-fullscreen)
(global-unset-key (kbd "C-z"))
(windmove-default-keybindings 'meta) ;; Move point from window to window
