;;;;;;;;;;;;;;
;; packages ;;
;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(load-file "~/.emacs.d/proxy.el")
(package-initialize)

(defvar my-packages '(
                      auctex
                      clojure-mode
                      clojure-test-mode
                      etags
                      etags-table
                      hackernews
                      highlight-symbol
                      markdown-mode
                      nrepl
                      p4
                      paredit
                      php-mode
                      starter-kit-eshell
                      yasnippet
                      xcscope
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;;;;;;;;
;; coding look ;;
;;;;;;;;;;;;;;;;;
(load-theme 'deeper-blue t)
(set-face-attribute 'default nil :font "Monaco" :height 115)
(global-hl-line-mode t)
(setq line-number-mode t)
(setq column-number-mode t)
(show-paren-mode t)
(set-cursor-color "yellow")
(setq-default cursor-type 'box)
(blink-cursor-mode t) ;; blinking cursor

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
(global-auto-revert-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq auto-save-timeout 60) ;; autosave every minute
(setq-default indicate-empty-lines t) ;; show empty lines
(setq scroll-preserve-screen-position t) ;; scroll without moving cursor
(setq next-line-add-newlines t) ;; add newline when at buffer end
(savehist-mode 1) ;; save minibuffer historic
(setq frame-title-format "%b - Emacs") ;; buffer name in the title bar
(setq icon-title-format "%b - Emacs") ;; buffer name in the title bar
(fset 'yes-or-no-p 'y-or-n-p) ;; yes-no shortcuts
(setq show-paren-style 'expression) ;; highlight text between parens
(delete-selection-mode t) ;; delete selected text
(setq x-stretch-cursor t) ;; cursor as wide as the character it is over
(electric-pair-mode t) ;; handle open/close brackets
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward) ;; overrides Emacs’ default mechanism for making buffer names unique
(setq-default save-place t) ;; When you visit a file, point goes to the last place where it was when you previously visited the same file
(setq visible-bell nil)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(define-key global-map (kbd "C-+") 'text-scale-increase) ;; increase font size
(define-key global-map (kbd "C--") 'text-scale-decrease) ;; decrease font size
(windmove-default-keybindings 'meta) ;; move point from window to window
(cua-mode t)
(setq cua-keep-region-after-copy t) ;; C-c, C-v and C-x default behavior
(setq next-line-add-newlines nil)
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-ido-prompt))

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
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8") ;; prefer utf-8 for language settings
(set-input-method nil) ;; no funky input for normal editing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; automatic and manual symbol highlighting for Emacs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)

;;;;;;;;;;;
;; email ;;
;;;;;;;;;;;
(setq user-mail-address "ludwig@lud.cc")

;;;;;;;;;;;;;
;; modules ;;
;;;;;;;;;;;;;
(load-file "~/.emacs.d/tags.el")
(load-file "~/.emacs.d/cpp.el")
(load-file "~/.emacs.d/modes.el")
(load-file "~/.emacs.d/latex.el")
(load-file "~/.emacs.d/p4.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(p4-depot-added-face ((t (:foreground "pale green"))))
 '(p4-depot-branched-face ((t (:foreground "green yellow"))))
 '(p4-diff-inserted-face ((t (:foreground "DodgerBlue1")))))
