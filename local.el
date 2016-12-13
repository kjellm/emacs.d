;;; local.el --- Random collection of useful stuff

;;; Commentary:

;;; Random collection of useful stuff

;;; Code:

(use-package f)
(use-package s)
(use-package dash
  :config
  (dash-enable-font-lock))

(defun local-buffer-project-root ()
  (projectile-root-bottom-up (buffer-file-name)))
(local-buffer-project-root)

(defun local-buffer-project-lib-dir ()
  (s-concat (local-buffer-project-root) "lib/"))

(defun local-buffer-file-name-relative-to-libdir ()
  (s-replace (local-buffer-project-lib-dir) "" (buffer-file-name)))

(defun local-rnest ()
  "For ruby files, insert in current buffer, based on it's file name, the proper module nesting."
  (interactive)
  (let* ((parts (f-split (f-no-ext (local-buffer-file-name-relative-to-libdir))))
         (camelized-parts (-map 's-upper-camel-case parts))
         (module-names (-drop-last 1 camelized-parts))
         (class-name (-last-item camelized-parts))
         (module-decls (--map (s-concat "module " it) module-names))
         (class-decl (s-concat "class " class-name))
         (str (s-join "\n" (-concat module-decls (list class-decl))))
         (end-str (s-join "\n" (--map "end" camelized-parts)))
         )
    (insert str)
    (insert "\n")
    (let* ((mid (point))
           )
      (insert end-str)
      (indent-region (point-min) (point))
      (goto-char mid)
      (end-of-line)
      )
    ))

(provide 'local)

;;; local.el ends here
