;;; init.el --- My Own Private Emacs Customizations

;;; Commentary:

;;; Code:

;; enable Melpa if it isn't enabled
(require 'package)
(when (not (assoc "melpa" package-archives))
  (setq package-archives (append '(("stable" . "https://stable.melpa.org/packages/")) package-archives))
  (setq package-archives (append '(("melpa" . "https://melpa.org/packages/")) package-archives))
  (setq package-archives (append '(("gnu" . "https://elpa..gnu.org/packages/")) package-archives))
  )
(package-initialize)

;; update package list
(when (not package-archive-contents) (package-refresh-contents))

;; install use-package if not there
(when (not (package-installed-p 'use-package))
  (package-installt 'use-package))

(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'after-init-hook 'global-company-mode)

(use-package yasnippet
  :ensure t)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
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

;; (use-package go-mode
;;   :ensure t
;;   :after(company-mode)
;;   :hook ((go-mode . lsp-deferred)
;; 	 (before-save . lsp-format-buffer)
;; 	 (before-save . lsp-organize-imports))
;;   :config
;;   (add-hook 'go-mode-hook 'lsp-deferred)
;;   (defun rb-go-mode-hook ()
;;     (setq tab-width 4)
;;     ;; (company-mode)
;;     (setq comment-auto-fill-only-comments t)
;;     (auto-fill-mode t)
;;     (lsp)
;;     (add-hook 'go-mode-hook 'flycheck-mode))

;;   (defun lsp-go-before-save-hooks ()
;;     (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;     (add-hook 'before-save-hook #'lsp-organize-imports t t))

;;   (add-hook 'go-mode-hook 'rb-go-mode-hook)
;;   (add-hook 'before-save-hook #'lsp-go-before-save-hooks))

;; Show current column in status bar
(column-number-mode 't)

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

;; configure tabs to be 4 spaces in nXML
(setq nxml-child-indent 4 nxml-attribute-indent 4)

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


;;(require 'flycheck)
;;(global-flycheck-mode 1)
;;(setq flycheck-checker-error-threshold 1000)

;; Project mgmt capabilities

(use-package projectile
  :ensure t
  ;;:bind-keymap
  ;;("C-c p" . projectile-command-map)
  )

;;(require 'projectile)
;;(projectile-mode "1.0")
;;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'git-commit)
(require 'magit)
(transient-append-suffix 'magit-push "-u"
  '(1 "-o" "Skip GitLab pipeline" "-o ci.skip"))

;; Set pinentry mode to loopback to be able to enter signing key
;; passphrase in minibuffer. For this to work, the GPG agent needs to
;; be configured to allow this as well.
(setq epa-pintenry-mode 'loopback)

;; Helm
(require 'helm)
(require 'helm-config)
(require 'helm-projectile)
(helm-mode 1)
(helm-projectile-on)

;;(require 'company)
;;(require 'yasnippet)
;; (yas-global-mode 1)

;; (require 'lsp-mode)

;; Org mode
;; have an additional state for DOING
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "DOING(g)" "ON-HOLD(o)" "|" "DONE(d)"))))

;; Go mode
;; (require 'go-mode)
;; (add-hook 'go-mode-hook #'lsp)
;; (add-hook 'go-mode-hook
;;	  (lambda ()
;;	    (setq tab-width 4)
;;	    (company-mode)
;;	    (setq comment-auto-fill-only-comments t)
;;	    (auto-fill-mode t)
;;	    ))

;; Terraform
(add-hook 'terraform-mode-hook
	  (lambda ()
	    (company-mode)
	    (require 'company-terraform)
	    (company-terraform-init)))

;; Preferences

(show-paren-mode t)

;; Markdown mode
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (setq whitespace-line-column 80)
	    (setq fill-column 80)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes '(tango-dark))
 '(inhibit-startup-screen t)
 '(lsp-ui-imenu-enable t)
 '(package-selected-packages
   '(gitlab-ci-mode-flycheck company-go dockerfile-mode toml-mode use-package company-terraform company terraform-mode lsp-mode yasnippet pinentry helm helm-projectile flycheck-golangci-lint flycheck-projectile go-mode go-projectile restclient restclient-test markdown-mode gitlab-ci-mode magit))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(url-debug t))


;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#2e3436" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "SF Mono")))))
