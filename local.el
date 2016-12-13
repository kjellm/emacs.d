;;; local.el --- Random collection of useful stuff

;;; Commentary:

;;; Random collection of useful stuff

;;; Code:

(use-package f)
(use-package s)
(use-package dash)

(defun local-buffer-project-root ()
  (projectile-root-bottom-up (buffer-file-name)))
(local-buffer-project-root)

(defun local-buffer-project-lib-dir ()
  (s-concat (local-buffer-project-root) "lib/"))

(defun local-file-name-relative-to-libdir ()
  (s-replace (local-buffer-project-lib-dir) "" (buffer-file-name)))

;; TODO: make last part a class
(defun local-rnest ()
  "For ruby files, insert in current buffer, based on it's file name, the proper module nesting."
  (interactive)
  (let* ((parts (f-split (f-no-ext (local-file-name-relative-to-libdir))))
         (camelized-parts (-map 's-upper-camel-case parts))
         (str (s-join "\n" (--map (s-concat "module " it) camelized-parts)))
         (end-str (s-join "\n" (--map "end" camelized-parts)))
         (beg (point))
         )
    (insert str)
    (insert "\n\n")
    (let ((mid (point))
          )
      (insert end-str)
      (indent-region beg (point))
      (goto-char mid))
    ))

(provide 'local)

;;; local.el ends here
