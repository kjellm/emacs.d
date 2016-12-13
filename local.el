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

(defun foobar ()
  "FIXME!"
  (interactive)
  (let* ((x (f-no-ext (local-file-name-relative-to-libdir)))
         (y (-map 's-upper-camel-case (f-split x)))
         (z (s-join "\n" (-map (lambda (s) (s-concat "module " s)) y)))
         (beg (point))
         )
    (insert z)
    (indent-region beg (point))
    ))
