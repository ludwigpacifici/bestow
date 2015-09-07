(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defvar my-packages '(
                      cider
                      clang-format
                      clojure-mode
                      cmake-font-lock
                      csv-mode
                      exec-path-from-shell
                      expand-region
                      flycheck
                      flycheck-clojure
                      ggtags
                      ido-ubiquitous
                      magit
                      markdown-mode
                      monokai-theme
                      paredit
                      scss-mode
                      smartparens
                      smex
                      solarized-theme
                      yaml-mode
                      zenburn-theme
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
