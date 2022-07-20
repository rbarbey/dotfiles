;;; init.el --- My Own Private Emacs Customizations

;;; Commentary:

;;; Code:

;; enable Melpa if it isn't enabled
(require 'package)
(when (not (assoc "melpa" package-archives))
  (setq package-archives (append '(("stable" . "https://stable.melpa.org/packages/")) package-archives))
  (setq package-archives (append '(("melpa" . "https://melpa.org/packages/")) package-archives))
  )
(package-initialize)

;; update package list
(when (not package-archive-contents) (package-refresh-contents))

;; install use-package if not there
(when (not (package-installed-p 'use-package))
  (package-installt 'use-package))

(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'after-init-hook 'global-company-mode)

(use-package diminish
  :ensure t)

(use-package yasnippet
  :ensure t
  :config (yas-global-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  (go-mode . lsp-deferred)
  (nxml-mode . lsp-deferred)
  :config (progn
	    ;; use flycheck, not flymake
	    (setq lsp-prefer-flymake nil)
	    (setq lsp-print-performance nil)
	    (setq lsp-log-io nil)))

(use-package lsp-ui
  :ensure t
  :after(lsp-mode)
  :commands lsp-ui-mode
  :config (progn
	    (setq lsp-ui-sideline-enable t)
	    (setq lsp-ui-doc-enable t)
	    (setq lsp-ui-imenu-enable t)
	    (setq lsp-ui-imenu-kind-position 'top))
  )

(use-package lsp-java
  :ensure t
  :config
  (add-hook 'java-mode-hook 'lsp)
  (add-hook 'java-mode-hook (lambda()
			      (setq c-basic-offset 4
				    tab-width 4
				    indent-tabs-mode nil))))

(use-package company
  :ensure t
  :config (progn
	    (setq company-idle-delay 0)
	    (setq company-minimum-prefix-length 2)
	    (setq company-tooltip-align-annotations t)
	    )
  )

(use-package flycheck
  :ensure t)

(use-package go-mode
  :mode ("\\.go" . go-mode)
  :init
  :hook (go-mode . yas-minor-mode)
  :config
  (defun rb-go-mode-hook ()
    "Basic Go mode setup"
    (setq tab-width 4)
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook 'rb-go-mode-hook))

;; Show current column in status bar
(use-package simple
  :config (column-number-mode 't))

;; Whitespaces
(global-whitespace-mode 1)
(setq
 whitespace-style
 '(face
   trailing
   lines-tail
   space-before-tab
   space-after-tab
   newline
   ; indentation
   empty
   )
 whitespace-line-column 100)

(when window-system
  (set-frame-size (selected-frame) 103 63))

(use-package nxml-mode
  :mode (("\\.xml$" . nxml-mode)
	 ("\\.xslt$" . nxml-mode)
	 ("\\.xsd$" . nxml-mode)
	 ("\\.plist$" . nxml-mode))
  :config
  (setq nxml-child-indent 4 nxml-attribute-indent 4))

;; PACKAGES


(use-package toml-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package gitlab-ci-mode-flycheck
  :after flycheck gitlab-ci-mode
  :ensure t
  :init
  (gitlab-ci-mode-flycheck-enable))

;; Project mgmt capabilities

(use-package projectile
  :ensure t
  :diminish (projectile-mode . "Proj")
  :init (projectile-mode +1)
  :config (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

(use-package go-projectile
  :ensure t
  :after projectile)

(require 'git-commit)
(require 'magit)
(transient-append-suffix 'magit-push "-u"
  '(1 "-o" "Skip GitLab pipeline" "-o ci.skip"))

;; Set pinentry mode to loopback to be able to enter signing key
;; passphrase in minibuffer. For this to work, the GPG agent needs to
;; be configured to allow this as well.
(setq epa-pintenry-mode 'loopback)

;; Helm
(use-package helm
  :ensure t
  :config (helm-mode))

(use-package helm-projectile
  :ensure t
  :after helm
  :config (helm-projectile-on))

(use-package helm-lsp
  :ensure t
  :after helm)

;;(require 'helm-config)
;;(require 'helm-projectile)
;;(helm-mode 1)
;;(helm-projectile-on)

;; Org mode
;; have an additional state for DOING
(use-package org
  :ensure t
  :config (setq org-todo-keywords
      (quote ((sequence "TODO(t)" "DOING(g)" "ON-HOLD(o)" "|" "DONE(d)")))))


;; Terraform
(use-package terraform-mode
  :ensure t
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package company-terraform
  :ensure t
  :after company-mode terraform-mode
  :init (company-terraform-init))

;; Preferences
(use-package paren
  :config (show-paren-mode t))

(use-package frame
  :config (blink-cursor-mode nil))

(use-package tool-bar
  :config (tool-bar-mode -1))

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :config (add-hook 'markdown-mode-hook
		    (lambda ()
		      (auto-fill-mode 1)
		      (setq whitespace-line-column 80)
		      (setq fill-column 80))))

;; Groovy mode for Gradle files
(use-package groovy-mode
  :ensure t
  :mode ("\\.gradle$" . groovy-mode))

;; create backup file in the Trash
(setq backup-directory-alist
      '((".*" . "~/.Trash")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tango-dark))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(gitlab-ci-mode-flycheck company-go dockerfile-mode toml-mode use-package company-terraform company terraform-mode lsp-mode yasnippet pinentry helm helm-projectile flycheck-golangci-lint flycheck-projectile go-mode go-projectile restclient restclient-test markdown-mode gitlab-ci-mode magit))
 '(url-debug t))


;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#2e3436" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "SF Mono")))))
