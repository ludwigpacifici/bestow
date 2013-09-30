(require 'tex-site)

(setq TeX-PDF-mode t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq reftex-plug-into-AUCTeX t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook (lambda () (TeX-fold-mode 1)))
(add-hook 'reftex-load-hook 'imenu-add-menubar-index)

(autoload 'LaTeX-preview-setup "preview")

