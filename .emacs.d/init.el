(setq gc-cons-threshold 100000000)

(defun rb/display-startup-time ()
  (message "Emacs loaded in %s with %d gcs"
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'rb/display-startup-time)

;; Basic visual setting

(global-whitespace-mode 1)
(global-subword-mode 1)
(setq whitespace-style
  '(spaces tabs newline space-mark tab-mark face trailing empty))

(setq inhibit-startup-message t) ;; No startup screen please
(scroll-bar-mode -1)             ;; no scroll bar
(tooltip-mode -1)                ;; no tooltips
(set-fringe-mode 10)             ;; ?
(menu-bar-mode -1)               ;; no menu bar
(setq visible-bell nil)          ;; no visual bell
(setq-default indent-tabs-mode nil)
(setq initial-major-mode 'fundamental-mode) ; changes *scratch* buffer mode
(setq initial-scratch-message "# This buffer is for notes you don't want to save\n\n")
(setq mac-right-option-modifier nil) ;; for mac-style umlaut input
(setq tags-table-list '("~/.emacs.d/.cache"))
(delete-selection-mode 1)

;; column numbers and auto fill
(use-package simple
  :ensure nil
  :config
  (column-number-mode 1))

;; no tool bar
(use-package tool-bar
  :ensure nil
  :config (tool-bar-mode -1))

;; use nice SF
(set-face-attribute 'default nil :font "SF Mono Light" :height 120)

;; initial size
(when window-system
  ;; position frame slightly off center
  (let* ((workarea (frame-monitor-workarea (selected-frame)))
         (width (caddr workarea))
         (height (cadddr workarea)))
  (set-frame-position (selected-frame)
                      (* (/ width 100) 50)
                      (/ (- height (frame-pixel-height (selected-frame))) 3)))

  ;; set initial size of frame
  (set-frame-size (selected-frame) 99 60))

(global-set-key (kbd "s-/") 'comment-or-uncomment-region)

;; delete until beginning of line
(defun rb/kill-to-beginning-of-line ()
  (interactive)
  (kill-region (point-at-bol) (point-at-eol)))

(global-set-key (kbd "s-<backspace>") #'rb/kill-to-beginning-of-line)

;; get Eclipse like line moving
(defun rb/move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun rb/move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "M-<up>") 'rb/move-line-up)
(global-set-key (kbd "M-<down>") 'rb/move-line-down)

;; get Eclipse like line duplication
(defun rb/duplicate-line ()
  "Duplicates current line and appends under the current one."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )

(global-set-key (kbd "M-s-<down>") 'rb/duplicate-line)

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
  ;; (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-create-missing-test-files t)
  )

;; follow compilation buffer
(setq compilation-scroll-output 1)

;; It's Magit!
(use-package magit
  :config
  (transient-append-suffix 'magit-push "-u"
                           '(1 "-o" "Skip CI pipeline" "-o ci.skip")))

;; Org mode
(use-package org
  :config
  (setq org-todo-keywords
        (quote ((sequence "TODO" "DOING" "|" "DONE"))))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-qagenda-files
        '("~/Documents/journal"))
  )

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

(use-package flycheck
  :config (global-flycheck-mode))

(use-package yasnippet
  :config (yas-global-mode))

(use-package dap-mode
  :custom
  (dap-auto-configure-features '(locals controls tooltip expressions))
  :config
  (add-hook 'dap-session-created-hook
            (lambda (session) (set-frame-size (selected-frame) 167 60)))
  (add-hook 'dap-terminated-hook
            (lambda (session) (set-frame-size (selected-frame) 99 60)))
  (bind-key (kbd "<f5>") #'dap-step-in dap-mode-map)
  (bind-key (kbd "<f6>") #'dap-next dap-mode-map)
  (bind-key (kbd "<f7>") #'dap-step-out dap-mode-map)
  (bind-key (kbd "<f8>") #'dap-continue dap-mode-map)
  )

(use-package lsp-java
  :hook (java-mode . lsp-deferred)
  :bind (:map lsp-mode-map
              ("s-l g y" ("type hierarchy" . lsp-java-type-hierarchy))
              ("s-1" . lsp-execute-code-action))
  :custom
  (tab-width 4)
  :init
  (setq lsp-java-vmargs (list
                         "-Xmx1G"
                         "-XX:+UseG1GC"
                         "-XX:+UseStringDeduplication"
                         "-javaagent:/Users/robert/.m2/repository/org/projectlombok/lombok/1.18.20/lombok-1.18.20.jar")
        ;; lsp-java-java-path "/Library/Java/JavaVirtualMachines/temurin-11.jdk/Contents/Home/bin/java"
        )
  :config
  (setq c-basic-offset 4
        lsp-java-format-settings-url (concat "file://" (file-truename (locate-user-emacs-file "eclipse-formatter.xml"))))
  (require 'dap-java)
  )

(use-package go-mode
  :mode ("\\.go" . go-mode)
  :init
  :hook (go-mode . lsp-deferred)
  :custom
  (tab-width 4)
  :config
  (defun rb-go-mode-hook ()
    "Basic Go mode setup"
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook 'rb-go-mode-hook)
  (add-hook 'go-mode-hook (lambda () (add-to-list 'company-backends '(company-capf :with company-yasnippet))))
  (require 'dap-dlv-go))

(use-package nxml-mode
  :ensure nil
  :hook (nxml-mode . lsp-deferred)
  :config
  (setq nxml-attribute-indent 4
        nxml-child-indent 4))

(use-package yaml-mode
  :bind
  (:map yaml-mode-map
        ("\C-m" . 'newline-and-indent)))

(use-package typescript-mode
  :custom (typescript-indent-level 2))

(use-package terraform-mode
  :hook (terraform-mode . lsp-deferred)
  :init (company-terraform-init))

(use-package company-terraform
  :defer t)

;; Restclient
(use-package restclient
  :mode ("\\.http" . restclient-mode))

;; Markdown
(use-package markdown-mode
  :ensure nil
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (set-face-attribute 'markdown-code-face nil :font "SF Mono Light" :height 120)
              (auto-fill-mode 1))))

;; MMorph programming
(add-to-list 'auto-coding-alist
             '("\\.mmo\\(rph\\)?$" . latin-9))
