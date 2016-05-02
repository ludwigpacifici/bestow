(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defvar my-packages '(
                      cider
                      clang-format
                      clojure-mode
                      cmake-font-lock
                      csv-mode
                      d-mode
                      exec-path-from-shell
                      expand-region
                      fireplace
                      flx
                      flx-ido
                      flycheck
                      flycheck-clojure
                      ggtags
                      ido-grid-mode
                      ido-ubiquitous
                      magit
                      markdown-mode
                      monokai-theme
                      paredit
                      rainbow-delimiters
                      scss-mode
                      smex
                      solarized-theme
                      typit
                      yaml-mode
                      zenburn-theme
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
