(require 'cc-mode)
(setq-default c-basic-offset 2)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun my-coding-hook ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t)
  (if window-system (hl-line-mode t))
  (idle-highlight-mode t))

(add-hook 'c++-mode-hook 'my-coding-hook)
