(when (display-graphic-p)
  (load-theme 'monokai t)
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
