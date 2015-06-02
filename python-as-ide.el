;; information form: https://github.com/jhamrick/emacs/blob/master/.emacs.d/settings/python-settings.el

(require 'python)

(eval-after-load 'python
  '(define-key python-mode-map (kbd "C-c !") 'python-shell-switch-to-shell))
(eval-after-load 'python
  '(define-key python-mode-map (kbd "C-c |") 'python-shell-send-region))
