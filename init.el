;;;;;;;;;;;;;;
;; packages ;;
;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(setq url-proxy-services '(("no_proxy" . "work\\.com")
                           ("http" . "proxy:3128")))
(package-initialize)

(defvar my-packages '(
                      clojure-mode
                      clojure-test-mode
                      etags
                      nrepl
                      p4
                      paredit
                      starter-kit-eshell
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;;;;;;;;
;; coding look ;;
;;;;;;;;;;;;;;;;;
(set-background-color "#fdf6e3")
(set-face-attribute 'default nil :font "Monaco" :height 115)
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
(ido-mode t)
(setq ido-enable-flex-matching t)
(iswitchb-mode t)
(savehist-mode 1) ;; save minibuffer historic
(setq frame-title-format "%b - Emacs") ;; buffer name in the title bar
(setq icon-title-format "%b - Emacs") ;; buffer name in the title bar
(fset 'yes-or-no-p 'y-or-n-p) ;; yes-no shortcuts
(delete-selection-mode t) ;; delete selected text
(setq x-stretch-cursor t) ;; cursor as wide as the character it is over
(electric-pair-mode t) ;; handle open/close brackets
(require 'uniquify) ;; overrides Emacs’ default mechanism for making buffer names unique
(setq uniquify-buffer-name-style 'forward)
(require 'saveplace) ;; When you visit a file, point goes to the last place where it was when you previously visited the same file
(setq-default save-place t)
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
(define-key global-map "\C-x\C-r" 'rgrep)

;;;;;;;;;;;;;;;;;;
;; coding style ;;
;;;;;;;;;;;;;;;;;;
(setq require-final-newline 't) ;; newline at the end of the file
(setq-default indent-tabs-mode nil) ;; no tabs
(setq tab-width 4) ;; tabs size

;;;;;;;;;;;;;;;
;; C++ style ;;
;;;;;;;;;;;;;;;
(setq-default c-basic-offset 2) ;; indent size
(setq c-default-style "k&r") ;; indent style

;;;;;;;;;;;;;;;;;;;;;;;;
;; Character encoding ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8") ;; prefer utf-8 for language settings
(set-input-method nil) ;; no funky input for normal editing

;;;;;;;;;;;;;;;;;
;; match paren ;;
;;;;;;;;;;;;;;;;;
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;;;;;;;;;;;;;
;; modules ;;
;;;;;;;;;;;;;
(load-file "~/.emacs.d/job.el")
(load-file "~/.emacs.d/tags.el")

