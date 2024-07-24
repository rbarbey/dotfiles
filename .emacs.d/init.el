;;; package --- Summary
;; Personal Emacs config

;;; Commentary:
;; Emacs configuration setting up LSP mode for Java, custom keymap
;; bindings, Magit, among other things.

;;; Code:

(setq gc-cons-threshold 100000000)

(defun rb/display-startup-time ()
  """Display startup time."""
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
(setq read-process-output-max (* 1024 1024 8))

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
(defun rb/set-font-faces ()
  (message "Setting font faces")
  (set-face-attribute 'default nil :font "SF Mono Light" :height 120)
  (set-face-attribute 'fixed-pitch nil :font "SF Mono Light" :height 120)
  (set-face-attribute 'variable-pitch nil :font "SF Mono Light" :height 120))

;; position frame slightly off center
(defun rb/set-frame-size (frame)
  (let* ((workarea (frame-monitor-workarea frame))
         (width (caddr workarea))
         (height (cadddr workarea)))
  (set-frame-position frame
                      (* (/ width 100) 50)
                      (/ (- height (frame-pixel-height frame)) 3)))

  ;; set initial size of frame
  (set-frame-size frame 99 60))

(when (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (rb/set-font-faces)
                (when (display-graphic-p frame)
                  (rb/set-frame-size frame)))))

(unless (daemonp)
  (rb/set-font-faces)
  (rb/set-frame-size (selected-frame)))

(global-set-key (kbd "s-/") 'comment-line)

;; delete until beginning of line
(defun rb/kill-to-beginning-of-line ()
  (interactive)
  (kill-region (pos-bol) (pos-eol)))

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
  "Duplicate current line and append under the current one."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )

(global-set-key (kbd "M-s-<down>") 'rb/duplicate-line)

;; point-to-register-to-point
(defun rb/point-excursion-toggle ()
  "Save position of point and return to previous point."
  (interactive)
  (cond ((get-register 0)
         (jump-to-register 0)
         (set-register 0 nil))
        (t(point-to-register 0)
          (message "Save point %d" (point)))))

(global-set-key (kbd "C-c c") #'rb/point-excursion-toggle)

;; for getting GITLAB token
(defun rb/lookup-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

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
  :config
  (ivy-mode 1)
  (setq ivy-use-selectable-prompt t))

(use-package counsel
  :bind (("M-x" . counsel-M-x)))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; Add icons to mode line
;; After this package got installed, you need to run the following command manually
;; M-x nerd-icons-install-fonts
;; Need to research whether there is an automatic way to do this.
(use-package nerd-icons)

;; Have a nice mode line
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 20)
  (doom-modeline-buffer-file-name-style 'file-name-with-project)
  (doom-modeline-percent-position nil))

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

(use-package ripgrep)

;; follow compilation buffer
(setq compilation-scroll-output 1)

;; It's Magit!
(use-package magit
  :config
  (transient-append-suffix 'magit-push "-u"
                           '(1 "-o" "Skip CI pipeline" "-o ci.skip")))

;; Org mode
(defun rb/init-org-mode ()
  (org-indent-mode))

(defun rb/org-archive-location ()
  "Returns location for archived tasks.
The string returns the filename where to store archived tasks. It
  contains the year and the month, for example
  `archive-2023-12.org`. The headline also contains month and year."
  (concat "archive-"
          (format-time-string "%Y-%m" (current-time))
          ".org::* Archived Tasks "
          (format-time-string "%B %Y" (current-time))))

(use-package org
  :hook (org-mode . rb/init-org-mode)
  :config
  (setq org-ellipsis " ↓")
  (setq org-todo-keywords
        '((sequence "TODO" "DOING" "|" "DONE")))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-archive-location (rb/org-archive-location))
  (setq org-agenda-files
        '("~/devel/agenda"
          "~/Documents/journal")))

;; Programming languages
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs)

(use-package lsp-docker)

(use-package lsp-ivy
  :after lsp-mode)

(use-package lsp-sonarlint
  :after lsp-mode
  :config
  (require 'lsp-sonarlint-java)
  (setq lsp-sonarlint-java-enabled t)
  (require 'lsp-sonarlint-go)
  (setq lsp-sonarlint-go-enabled t))


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
  :after lsp-treemacs
  :bind (:map dap-mode-map
              ("<f5>" . dap-step-in)
              ("<f6>" . dap-next)
              ("<f7>" . dap-step-out)
              ("<f8>" . dap-continue))
  :custom
  (dap-auto-configure-features '(locals controls tooltip expressions))
  :config
  (add-hook 'dap-session-created-hook
            (lambda (session) (set-frame-size (selected-frame) 167 60)))
  (add-hook 'dap-terminated-hook
            (lambda (session) (set-frame-size (selected-frame) 99 60)))
  )

(use-package lsp-java
  :hook (java-mode . lsp-deferred)
  :bind (:map lsp-mode-map
              ("s-l g y" . lsp-java-type-hierarchy)
              ("s-1" . lsp-execute-code-action))
  :custom
  (tab-width 4)
  :init
  (setq lsp-java-vmargs (list
                         "-Xmx2G"
                         "-XX:+UseG1GC"
                         "-XX:+UseStringDeduplication"
                         )
        ;; lsp-java-java-path "/Library/Java/JavaVirtualMachines/temurin-11.jdk/Contents/Home/bin/java"
        )
  (which-key-add-key-based-replacements "s-l g y" "type hierarchy")
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

(use-package js
  :ensure nil
  :custom (js-indent-level 2))

(use-package terraform-mode
  :hook
  (terraform-mode . lsp-deferred)
  (terraform-mode . terraform-format-on-save-mode)
  :init (company-terraform-init))

(use-package company-terraform
  :defer t)

;; tree-sitter
(with-eval-after-load 'treesit
  (dolist (lang-sources
           '((go "https://github.com/tree-sitter/tree-sitter-go")
             (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
             (java "https://github.com/tree-sitter/tree-sitter-java")
             (markdown "https://github.com/ikatyang/tree-sitter-markdown")
             (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
    (add-to-list 'treesit-language-source-alist lang-sources)))

;; Restclient
(use-package restclient
  :mode ("\\.http" . restclient-mode))

;; Markdown
(use-package markdown-mode
  :ensure nil
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (auto-fill-mode 1))))

;; GitLab

(use-package gitlab-ci-mode)

(use-package gitlab-ci-mode-flycheck
  :after flycheck gitlab-ci-mode
  :init (gitlab-ci-mode-flycheck-enable)
  :custom (gitlab-ci-api-token (rb/lookup-password :host "gitlab.com")))

;; MMorph programming
(add-to-list 'auto-coding-alist
             '("\\.mmo\\(rph\\)?$" . latin-9))

;;; init.el ends here
