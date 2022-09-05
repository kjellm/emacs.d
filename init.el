;;; init.el --- Emacs initialization

;;; Commentary:

;;; TODO
;; - enclose region in parens, quotes etc
;; - javascript
;;

;;; Code:

;;; Packages

(push "~/.emacs.d/use-package" load-path)
(require 'use-package)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(setq use-package-always-ensure t)

(setq ring-bell-function 'ignore)

;;; Mac fixes

(setq default-input-method "MacOSX")
(setq mac-command-modifier 'meta
      mac-option-modifier nil
      mac-allow-anti-aliasing t
      mac-command-key-is-meta t)
(menu-bar-mode +1)

;; On OS X Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;; UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(setq inhibit-startup-screen t)
(show-paren-mode t)

(require 'windmove)
(windmove-default-keybindings)

(use-package solarized-theme)
(use-package zenburn-theme)
(use-package gotham-theme)
;(load-theme 'solarized-light t)
(load-theme 'solarized-dark t)
;(load-theme 'gotham t)
;(load-theme 'zenburn t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(transient-mark-mode t)

;;; Editor

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file  'noerror)

(setq require-final-newline t)

(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-auto-revert-mode t)

(use-package projectile
  :config
  (projectile-global-mode t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ido
  :config (ido-mode t))

(use-package flx-ido
  :config (flx-ido-mode 1))

(use-package smex
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(use-package project-explorer)

(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing lines-tail))
(add-hook 'before-save-hook 'whitespace-cleanup)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package ag
  :bind
  ("C-c p s a" . ag-project))

(use-package guru-mode
  :config
  (guru-global-mode))

;;; Writing

(setq ispell-program-name "aspell") ; ispell, hunspell

(add-hook 'text-mode-hook (flyspell-mode +1))

(use-package markdown-mode)

(use-package deft)
(setq deft-directory "~/work/fagansvarlig/docs")

;;; Programming

(use-package flycheck)

(defun local-prog-mode-hook ()
  (flycheck-mode)
  (flyspell-prog-mode)
  (whitespace-mode)
  (subword-mode 1))

(add-hook 'prog-mode-hook 'local-prog-mode-hook)


;; Ruby

(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))

(use-package rbenv
  :config
  (global-rbenv-mode)
  (rbenv-use-corresponding))

(setq ruby-insert-encoding-magic-comment nil)

;; Elixir

(use-package elixir-mode)
(use-package alchemist)

;; Elm

(use-package elm-mode
  :init
  (setq elm-format-on-save t))

;; JavaScript

(use-package js2-mode
  :config (setq js2-basic-offset 2
              js2-highlight-level 3)
  :mode (("\\.js$" . js2-mode)))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; Other

(require 'nxml-mode)
(require 'sh-script)
(use-package yaml-mode)

; web-mode
(use-package web-mode
  :config (setq web-mode-markup-indent-offset 2)
  :mode (("\\.hbs$" . web-mode)))

;;; VC

(use-package magit
  :bind
  ("C-x g" . magit-status))


(push "~/.emacs.d/lisp" load-path)
(require 'local)
