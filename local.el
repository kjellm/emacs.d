;;; local.el --- Random collection of useful stuff

;;; Commentary:

;;; Random collection of useful stuff

;;; Code:

(use-package f)
(use-package s)
(use-package dash)

(defun foobar (buffername)
  "FIXME!"
  (interactive "b")
  (let* ((fname (buffer-file-name (get-buffer buffername)))
         (proot (projectile-root-bottom-up fname))
         (libdir (s-concat proot "lib/"))
         (x (f-dirname (s-replace libdir "" fname)))
         (y (-map 's-upper-camel-case (f-split x)))
         (z (s-join "\n" (-map (lambda (s) (s-concat "module " s)) y)))
         (beg (point))
         )
    (insert z)
    (indent-region beg (point))
    ))
