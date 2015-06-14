;; information form: https://github.com/jhamrick/emacs/blob/master/.emacs.d/settings/python-settings.el

(require 'python)

(setq python-shell-interpreter "ipython")

(eval-after-load 'python
  '(define-key python-mode-map (kbd "C-c s") 'python-shell-switch-to-shell))
(eval-after-load 'python
  '(define-key python-mode-map (kbd "C-c r") 'python-shell-send-region))
(eval-after-load 'python
  '(define-key python-mode-map (kbd "C-c f") 'python-shell-send-file))
