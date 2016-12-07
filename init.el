;;; Mac fixes

(setq default-input-method "MacOSX")
(setq mac-command-modifier 'meta
      mac-option-modifier nil
      mac-allow-anti-aliasing t
      mac-command-key-is-meta t)
(menu-bar-mode +1)

;;; Packages

(push "~/.emacs.d/use-package" load-path)
(require 'use-package)

(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(setq use-package-always-ensure t)

;;; UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(setq inhibit-startup-screen t)
(show-paren-mode t)

(require 'windmove)
(windmove-default-keybindings)

(use-package solarized-theme)
(load-theme 'solarized-dark t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;;; Editor

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file  'noerror)

(setq require-final-newline t)

(setq make-backup-files nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(use-package projectile
  :config (projectile-global-mode t))

(use-package ido
  :config (ido-mode t))

(use-package smex
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing lines-tail))
(add-hook 'before-save-hook 'whitespace-cleanup)

;;; Writing

(use-package markdown-mode)

;;; Programming

(use-package flycheck)

(defun local-prog-mode-hook ()
  (flycheck-mode)
  (flyspell-prog-mode)
  (whitespace-mode))

(add-hook 'prog-mode-hook 'local-prog-mode-hook)


;; Ruby

(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.cap\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Podfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.podspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Puppetfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Appraisals\\'" . ruby-mode))

(use-package rbenv)
(global-rbenv-mode)
(rbenv-use-corresponding)

(setq ruby-insert-encoding-magic-comment nil)

;; Other

(require 'nxml-mode)
(require 'sh-script)
(use-package yaml-mode)

;;; VC

(use-package magit
  :bind
  ("C-x g" . magit-status))
