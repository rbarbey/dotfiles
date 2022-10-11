(defun rb/display-startup-time ()
  (message "Emacs loaded in %s with %d gcs"
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'rb/display-startup-time)

;; Basic visual setting

(global-whitespace-mode 1)
(setq whitespace-style
  '(spaces tabs newline space-mark tab-mark face trailing empty))

(setq inhibit-startup-message t) ;; No startup screen please
(scroll-bar-mode -1)             ;; no scroll bar
(tool-bar-mode -1)               ;; no tool bar
(tooltip-mode -1)                ;; no tooltips
(set-fringe-mode 10)             ;; ?
(menu-bar-mode -1)               ;; no menu bar
(setq visible-bell nil)          ;; no visual bell
(setq-default indent-tabs-mode nil)

;; show column numbers
(column-number-mode)

;; use nice SF
(set-face-attribute 'default nil :font "SF Mono Light" :height 120)

(global-set-key (kbd "s-/") 'comment-or-uncomment-region)

;; A nice theme is Tango Dark
;; (load-theme 'tango-dark)
;; Another nice theme is wombat
(load-theme 'wombat)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package management
(require 'package)

;; Add package repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" .  "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))


;; Enable hiding of minor modes from mode line
(use-package diminish)

;; set up Ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper))
  :config (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; Add icons to mode line
;; After this package got installed, you need to run the following command manually
;; M-x all-the-icons-install-fonts
;; Need to research whether there is an automatic way to do this.
(use-package all-the-icons)

;; Have a nice mode line
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 20)))

;; Rainbow delimiters: display matching parens according to their
;; level of nesting in slightly different colours
;; not sure yet if I want to keep this
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; show all available completions of a shortcuts
;; helpful but can also be annoying
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 1))

;; Project management using Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap ("s-p" . projectile-command-map)
  :init
  (setq projectile-switch-project-action #'projectile-dired))

;; follow compilation buffer
(setq compilation-scroll-output 1)

;; It's Magit!
(use-package magit)

;; Programming languages
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-ivy
  :after lsp-mode)

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection)
        ("<escape>" . company-abort))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package flycheck)

(use-package lsp-java
  :hook (java-mode . lsp-deferred)
  :custom
  (tab-width 4)
  :config
  (setq c-basic-offset 4))

(use-package go-mode
  :mode ("\\.go" . go-mode)
  :init
  :hook (go-mode . lsp-deferred)
  :custom
  (tab-width 4)
  (indent-tabs-mode t)
  :config
  (defun rb-go-mode-hook ()
    "Basic Go mode setup"
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook 'rb-go-mode-hook))

(use-package terraform-mode
  :hook (terraform-mode . lsp-deferred)
  :init (company-terraform-init))

(use-package company-terraform
  :defer t)

;; Restclient
(use-package restclient
  :mode ("\\.http" . restclient-mode))
