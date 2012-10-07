(push "/usr/local/bin" exec-path)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get") 
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s) (goto-char (point-max)) (eval-print-last-sexp))))

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)

(fset 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(show-paren-mode t)
(column-number-mode t)
(set-fringe-style 'default)
(tooltip-mode -1)
(setq require-final-newline t)
(setq mouse-yank-at-point t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq-default fill-column 78)

(set-frame-font "Menlo-12")

(set-background-color "DodgerBlue4")
(set-foreground-color "sky blue")
(set-face-foreground font-lock-comment-face "orange1")
(set-face-background 'fringe "black")

(global-set-key [(f6)] 'goto-line)
(global-set-key "\M-/" 'hippie-expand)

(setq default-input-method "MacOSX")
(setq mac-command-modifier 'meta
      mac-option-modifier nil
      mac-allow-anti-aliasing t
      mac-command-key-is-meta t)

;(require 'ido)
(setq ido-enable-flex-matching t)
(ido-mode t)

;;;; PERL
(add-to-list 'auto-mode-alist '("\\.t$" . perl-mode))
(defalias 'perl-mode 'cperl-mode)

(custom-set-variables
 '(cperl-close-paren-offset -4)
 '(cperl-continued-statement-offset 4)
 '(cperl-brace-offset 0)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(cperl-tab-always-indent t))

(setq cperl-invalid-face (quote off)) 


;;;;

(defun shell-mode-hook ()
  (make-local-variable 'tab-width)
  (setq tab-width 8)
)

(defun ruby-mode-hook ()
  (autoload 'ruby-mode "ruby-mode" nil t)
  (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
  (add-hook 'ruby-mode-hook '(lambda ()
                               (setq ruby-deep-arglist t)
                               (setq ruby-deep-indent-paren nil)
                               (setq c-tab-always-indent nil)
                               (require 'inf-ruby)
                               (require 'ruby-compilation)
                               (define-key ruby-mode-map (kbd "M-r") 'run-rails-test-or-ruby-buffer))))
(defun rhtml-mode-hook ()
  (autoload 'rhtml-mode "rhtml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
  (add-to-list 'auto-mode-alist '("\\.rjs\\'" . rhtml-mode))
  (add-hook 'rhtml-mode '(lambda ()
                           (define-key rhtml-mode-map (kbd "M-s") 'save-buffer))))

(defun yaml-mode-hook ()
  (autoload 'yaml-mode "yaml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))


;(setq cssm-indent-function #'cssm-c-style-indenter)
;(setq cssm-indent-level 4)
(defun css-mode-hook ()
  (autoload 'css-mode "css-mode" nil t)
  (add-hook 'css-mode-hook '(lambda ()
                              (setq css-indent-level 2)
                              (setq css-indent-offset 2))))


;;;;


(defun is-rails-project ()
  (when (textmate-project-root)
    (file-exists-p (expand-file-name "config/environment.rb" (textmate-project-root)))))

(defun run-rails-test-or-ruby-buffer ()
  (interactive)
  (if (is-rails-project)
      (let* ((path (buffer-file-name))
             (filename (file-name-nondirectory path))
             (test-path (expand-file-name "test" (textmate-project-root)))
             (command (list ruby-compilation-executable "-I" test-path path)))
        (pop-to-buffer (ruby-compilation-do filename command)))
    (ruby-compilation-this-buffer)))


;;;;


(require 'package)
(setq package-archives (cons '("tromey" . "http://tromey.com/elpa/") package-archives))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)

(setq el-get-sources
      '((:name ruby-mode 
               :type elpa
               :load "ruby-mode.el"
               :after (lambda () (ruby-mode-hook)))
        (:name inf-ruby  :type elpa)
        (:name ruby-compilation :type elpa)
        (:name css-mode 
               :type elpa 
               :after (lambda () (css-mode-hook)))
        (:name textmate
               :type git
               :url "git://github.com/defunkt/textmate.el"
               :load "textmate.el")
        (:name rvm
               :type git
               :url "http://github.com/djwhitt/rvm.el.git"
               :load "rvm.el"
               :compile ("rvm.el")
               :after (lambda() (rvm-use-default)))
        (:name rhtml
               :type git
               :url "https://github.com/crazycode/rhtml.git"
               :features rhtml-mode
               :after (lambda () (rhtml-mode-hook)))
        (:name el-get
               :type git
               :url "https://github.com/dimitri/el-get.git")
        (:name yaml-mode 
               :type git
               :url "http://github.com/yoshiki/yaml-mode.git"
               :features yaml-mode
               :after (lambda () (yaml-mode-hook)))
        (:name markdown-mode
               :description "Major mode to edit Markdown files in Emacs"
               :type git
               :url "git://jblevins.org/git/markdown-mode.git"
               :post-init (lambda ()
                            (add-to-list 'auto-mode-alist
                                         '("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode))))
        ;(:name yasnippet)
        (:name puppet-mode
               :description "A simple mode for editing puppet manifests"
               :type http
               :url "http://projects.puppetlabs.com/projects/puppet/repository/revisions/master/raw/ext/emacs/puppet-mode.el"
               :post-init (lambda ()
                            (autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests" t)
                            (add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))))
	))

(setq my-packages
      (append
       '(el-get magit)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)
