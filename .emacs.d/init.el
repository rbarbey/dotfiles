;; Basic visual setting

(setq inhibit-startup-message t) ;; No startup screen please
(scroll-bar-mode -1)             ;; no scroll bar
(tool-bar-mode -1)               ;; no tool bar
(tooltip-mode -1)                ;; no tooltips
(set-fringe-mode 10)             ;; ?
(menu-bar-mode -1)               ;; no menu bar
(setq visible-bell nil)          ;; no visual bell

;; show column numbers
(column-number-mode)

;; use nice SF
(set-face-attribute 'default nil :font "SF Mono Light" :height 120)

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
  (when (file-directory-p "~/devel")
	(setq projectile-search-path '("~/devel")))
  (setq projectile-switch-project-action #'projectile-dired))

;; follow compilation buffer
(setq compilation-scroll-output 1)
