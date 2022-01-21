;; PRODUCTIVITY

;; Show current column in status bar
(column-number-mode 't)

;; Whitespaces
(global-whitespace-mode)
(setq
 whitespace-style
 '(face
   trailing
   lines-tail
   space-before-tab
   space-after-tab
   newline
   indentation
   empty
   )
 whitespace-line-column 100)

;; PACKAGES
(package-initialize)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(require 'flycheck)
(global-flycheck-mode 1)
(setq flycheck-checker-error-threshold 1000)

;; Project mgmt capabilities
(require 'projectile)
(projectile-mode "1.0")
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'git-commit)

;; Helm
(require 'helm)
(require 'helm-config)
(require 'helm-projectile)
(helm-mode 1)
(helm-projectile-on)

;; Preferences

(menu-bar-mode 0)
(show-paren-mode t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(helm helm-projectile flycheck-golangci-lint flycheck-projectile go-mode go-projectile restclient restclient-test markdown-mode gitlab-ci-mode magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
