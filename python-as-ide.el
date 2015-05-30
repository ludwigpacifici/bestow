;; information form: https://github.com/jhamrick/emacs/blob/master/.emacs.d/settings/python-settings.el

(require 'python)
(require 'pydoc)
(require 'pydoc-info)

(setq-default py-shell-name "/usr/local/bin/ipython3")
(setq python-shell-interpreter "/usr/local/bin/ipython3")

(eval-after-load 'python
  '(define-key python-mode-map (kbd "C-c !") 'python-shell-switch-to-shell))
(eval-after-load 'python
  '(define-key python-mode-map (kbd "C-c |") 'python-shell-send-region))
