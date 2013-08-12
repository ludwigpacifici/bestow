;;;;;;;;;;;;;;
;; perforce ;;
;;;;;;;;;;;;;;
(load-library "p4")

;;;;;;;;;;;;;;;;;;
;; coding style ;;
;;;;;;;;;;;;;;;;;;
(setq require-final-newline 't) ;; newline at the end of the file
(setq-default indent-tabs-mode nil) ;; no tabs
(setq tab-width 3) ;; tabs size

;;;;;;;;;;;;;;;
;; C++ style ;;
;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.r\\'" . c-mode))
