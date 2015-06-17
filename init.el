(load-file "~/.emacs.d/init-packages.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac Os X environment variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))

;;;;;;;;;;;;;;;;
;; Appearance ;;
;;;;;;;;;;;;;;;;
(load-theme 'monokai t)
(set-face-attribute 'default nil :family "Monaco")
(set-face-attribute 'default nil :height 145)

;;;;;;;;;;;;;;
;; behavior ;;
;;;;;;;;;;;;;;
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(global-hl-line-mode t) ;; Highlight current line
(setq x-stretch-cursor t) ;; Wide cursor on tabs
(setq ring-bell-function 'ignore) ;; Turn off alarms
(line-number-mode t)
(column-number-mode t)
(setq-default cursor-type 'box)
(blink-cursor-mode t)
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
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

;;;;;;;;;;;;;;;;;;
;; coding style ;;
;;;;;;;;;;;;;;;;;;
(setq require-final-newline 't) ;; newline at the end of the file
(setq-default indent-tabs-mode nil) ;; no tabs
(setq tab-width 2) ;; tabs size
(setq indent-tabs-mode nil)

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
(setq org-completion-use-ido t)
(setq magit-completing-read-function 'magit-ido-completing-read)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

(require 'smex) ; Not needed if you use package.el
(smex-initialize)

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

(setq magit-last-seen-setup-instructions "1.4.0")

(require 'flymake-cppcheck)
(add-hook 'c-mode-hook 'flymake-cppcheck-load)
(add-hook 'c++-mode-hook 'flymake-cppcheck-load)
(setq flymake-cppcheck-enable "all")

;;;;;;;;;;;;;;;
;; shortcuts ;;
;;;;;;;;;;;;;;;
(windmove-default-keybindings 'meta) ;; Move point from window to window
(global-set-key [f11] 'toggle-frame-fullscreen)
(global-set-key "\M- " 'hippie-expand)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key  "\C-cc" 'compile)
(global-set-key [C-M-tab] 'clang-format-region)

;;;;;;;;;;;;;;
;; includes ;;
;;;;;;;;;;;;;;
(load-file "~/.emacs.d/init-clang-format.el")
(load-file "~/.emacs.d/init-compile.el")
(load-file "~/.emacs.d/init-cpp.el")
(load-file "~/.emacs.d/init-gnu-global.el")
(load-file "~/.emacs.d/init-python.el")
(load-file "~/.emacs.d/init-smartparens.el")
