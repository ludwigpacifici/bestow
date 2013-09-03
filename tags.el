(setq path-to-ctags "C:/cygwin/bin/ctags.exe") ;; ctags path

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R %s &" path-to-ctags dir-name (directory-file-name dir-name)))
  )


;;;;;;;;;;;;;;;;
;; EtagsTable ;;
;;;;;;;;;;;;;;;;
(setq etags-table-search-up-depth 10)

