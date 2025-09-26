;;; init.el --- Personal Emacs configuration
;;; Commentary:
;;; Kjell-Magne Øieurd's Emacs configuration
;;; Code:

;; ============================================================================
;; PACKAGE MANAGEMENT
;; ============================================================================

(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ;; uncomment if you want more stability
        ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
        ))
(package-initialize)
(require 'use-package)

;; ============================================================================
;; PLATFORM-SPECIFIC CONFIGURATION
;; ============================================================================

;; macOS fixes - better key handling
(when (eq system-type 'darwin)
  (setq ns-command-modifier 'meta
        ns-option-modifier  nil))

;; ============================================================================
;; BASIC EMACS SETTINGS
;; ============================================================================

;; UI Cleanup
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(setq inhibit-startup-screen t)

;; Editor behavior
(column-number-mode t)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(global-auto-revert-mode t)
(setq require-final-newline t)
(fset 'yes-or-no-p 'y-or-n-p)

;; ============================================================================
;; EDITOR CONFIG & STANDARDS
;; ============================================================================

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; ============================================================================
;; WINDOW MANAGEMENT
;; ============================================================================

;; Shift + arrow keys for moving between windows
(require 'windmove)
(windmove-default-keybindings)

;; ============================================================================
;; THEMES & APPEARANCE
;; ============================================================================

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (optional)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  ;; Load the theme you want
  (load-theme 'doom-one t))

;; ============================================================================
;; SYNTAX CHECKING & CODE QUALITY
;; ============================================================================

;; Real-time syntax checking
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

;; Spell-check in comments/strings
(use-package flyspell
  :ensure t
  :hook (prog-mode . flyspell-prog-mode))

;; Highlight problem whitespace
(use-package whitespace
  :config
  (setq whitespace-style '(face lines-tail)
        whitespace-line-column 80)

  ;; tweak the face for overflow beyond col 80
  (set-face-attribute 'whitespace-line nil
    :background "#593939"
    :foreground nil)
  
  :hook (prog-mode . whitespace-mode))

;; CamelCase word navigation
(use-package subword
  :hook (prog-mode . subword-mode))

;; ============================================================================
;; LEARNING & HABITS
;; ============================================================================

;; Discourage arrow keys and other non-Emacs habits
(use-package guru-mode
  :ensure t
  :config
  (guru-global-mode))

;; ============================================================================
;; VERSION CONTROL
;; ============================================================================

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))

(use-package magit-todos
  :ensure t
  :after magit
  :config
  (magit-todos-mode 1))

;; ============================================================================
;; LSP & CODE INTELLIGENCE
;; ============================================================================

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-prefer-flymake nil)
  ;; Optional: reduce LSP verbosity
  ;; (setq lsp-headerline-breadcrumb-enable nil)
  ;; (setq lsp-signature-auto-activate nil)
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  ;; Optional: customize LSP UI features
  ;; :config
  ;; (setq lsp-ui-doc-enable t
  ;;       lsp-ui-peek-enable t
  ;;       lsp-ui-sideline-enable t)
  )

;; Auto-completion
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  ;; Optional: customize completion behavior
  ;; :config
  ;; (setq company-idle-delay 0.2
  ;;       company-minimum-prefix-length 2)
  )

;; ============================================================================
;; MODERN COMPLETION FRAMEWORK
;; ============================================================================

;; Vertical completion interface
(use-package vertico
  :ensure t
  :init (vertico-mode))

;; Better M-x history and more
(use-package savehist
  :init (savehist-mode 1))

;; Flexible completion matching
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;; Rich annotations in completion
(use-package marginalia
  :ensure t
  :init (marginalia-mode))

;; Enhanced commands using completion
(use-package consult
  :ensure t)

;; ============================================================================
;; PROJECT MANAGEMENT (SUGGESTED)
;; ============================================================================

;; Uncomment to enable project-aware operations
;; (use-package projectile
;;   :ensure t
;;   :init
;;   (projectile-mode +1)
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :config
;;   ;; Use ripgrep when available
;;   (when (executable-find "rg")
;;     (setq projectile-generic-command "rg --files --color=never --null")))

;; Consult + Projectile integration
;; (use-package consult-projectile
;;   :ensure t
;;   :after (consult projectile))

;; ============================================================================
;; SEARCH & NAVIGATION
;; ============================================================================

;; Fast text search with ripgrep
(use-package ripgrep
  :ensure t)

;; ============================================================================
;; LANGUAGE-SPECIFIC CONFIGURATION
;; ============================================================================

;; Go development
(use-package go-mode
  :ensure t
  :hook ((go-mode . lsp-deferred)
         (before-save . gofmt-before-save)))

(use-package yaml-mode
  :ensure t)

;; ============================================================================
;; DISCOVERABILITY & HELP
;; ============================================================================

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  ;; Optional: customize which-key behavior
  ;; (setq which-key-idle-delay 0.5
  ;;       which-key-max-display-columns 5)
  )

;; ============================================================================
;; KEY BINDINGS
;; ============================================================================

;; Core navigation and search
(global-set-key (kbd "C-s")     #'consult-line)        ; in-buffer search
(global-set-key (kbd "C-x b")   #'consult-buffer)      ; switch buffers
(global-set-key (kbd "M-y")     #'consult-yank-pop)    ; kill-ring
(global-set-key (kbd "C-x C-r") #'consult-recent-file) ; recent files

;; Project-wide search
(global-set-key (kbd "C-c s") #'consult-ripgrep)

;; Optional: additional useful bindings
;; (global-set-key (kbd "C-c f") #'consult-find)         ; find files
;; (global-set-key (kbd "C-c g") #'consult-goto-line)    ; go to line
;; (global-set-key (kbd "C-c i") #'consult-imenu)        ; navigate symbols
;; (global-set-key (kbd "C-c o") #'consult-outline)      ; navigate headings

;; Alternative: keep original C-s behavior and add consult-line elsewhere
;; (global-set-key (kbd "C-c l") #'consult-line)

;; Optional: project-specific bindings (requires projectile)
;; (global-set-key (kbd "C-c p s") #'consult-projectile-ripgrep)
;; (global-set-key (kbd "C-c p f") #'consult-projectile-find-file)

;; ============================================================================
;; AI ASSISTANCE (EXPERIMENTAL)
;; ============================================================================

;; GitHub Copilot integration
;; (use-package copilot
;;   :ensure t
;;   :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-map
;;          ("C-<tab>" . 'copilot-accept-completion)
;;          ("C-TAB" . 'copilot-accept-completion)
;;          ("C-<right>" . 'copilot-accept-completion-by-word)
;;          ("C-<down>" . 'copilot-next-completion)
;;          ("C-<up>" . 'copilot-previous-completion)))
;; ============================================================================
;; CUSTOM VARIABLES & FACES
;; ============================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company consult doom-themes flycheck go-mode guru-mode lsp-mode
	     lsp-ui magit magit-todos marginalia orderless ripgrep
	     vertico yaml-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
