;;; init.el --- My Own Private Emacs Customizations

;;; Commentary:

;;; Code:

;; PRODUCTIVITY

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

;; PACKAGES
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'flycheck)
(global-flycheck-mode 1)
(setq flycheck-checker-error-threshold 1000)

;; Project mgmt capabilities
(require 'projectile)
(projectile-mode "1.0")
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'git-commit)

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

(require 'company)
(require 'yasnippet)
(yas-global-mode 1)

(require 'lsp-mode)

;; Go mode
(require 'go-mode)
(add-hook 'go-mode-hook #'lsp)
(add-hook 'go-mode-hook (lambda ()
			  (setq tab-width 4)))

;; Terraform
(add-hook 'terraform-mode-hook
	  (lambda ()
	    (company-mode)
	    (require 'company-terraform)
	    (company-terraform-init)))

;; Preferences

(menu-bar-mode 0)
(show-paren-mode t)

;; Adjust colors
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-function-name-face ((t (:bold t :foreground "#0123f5")))))

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
 '(package-selected-packages
   '(company-terraform company terraform-mode lsp-mode yasnippet pinentry helm helm-projectile flycheck-golangci-lint flycheck-projectile go-mode go-projectile restclient restclient-test markdown-mode gitlab-ci-mode magit)))


;;; init.el ends here
