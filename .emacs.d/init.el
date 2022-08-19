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
  :config (ivy-mode 1))

;; Have a nice mode line
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 20)))

;; Rainbow delimiters: display matching parens according to their
;; level of nesting in slightly different colours
;; not sure yet if I want to keep this
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
