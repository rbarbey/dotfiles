;;; init.el ---- -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs configuration setting up LSP mode for Java, custom keymap
;; bindings, Magit, among other things.

;;; Code:

(defun rb/display-startup-time ()
  "Display startup time."
  (message "Emacs loaded in %s with %d gcs"
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'rb/display-startup-time)

;; Initialize package management
(require 'package)

;; Add package repositories
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org"   .  "https://orgmode.org/elpa/")
        ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; put backup, auto save and lock files into temp directory
(setq backup-directory-alist '(("." . "/tmp")))
(setq auto-save-file-name-transforms `((".*" ,"/tmp/" t)))
(setq lock-file-name-transforms '(("\\`/.*/\\([^/]+\\)\\'" "/tmp/\\1" t)))

;; No startup screen please
(setq inhibit-startup-message t)

;; whitespace settings
(global-whitespace-mode 1)
(global-subword-mode 1)
(setq whitespace-style
  '(spaces tabs newline space-mark tab-mark face trailing empty))
(setq-default indent-tabs-mode nil)
(delete-selection-mode 1)
(setq sentence-end-double-space nil)
(setq-default fill-column 78)

;; Basic visual setting

;; configure scrolling behaviour
(setopt scroll-bar-mode nil
        scroll-preserve-screen-position nil)

(scroll-bar-mode -1)             ;; no scroll bar
(tooltip-mode -1)                ;; no tooltips
(set-fringe-mode 10)             ;; ?
;; (menu-bar-mode -1)               ;; no menu bar
(setq visible-bell nil)          ;; no visual bell

;; line numbers
(column-number-mode 1)
(use-package display-line-numbers
  :hook (prog-mode-hook . display-line-numbers-mode)
  :config
  :custom
  (display-line-numbers-width 3))

; configure *scratch* buffer
(setq initial-major-mode 'fundamental-mode)

(defun rb/c64-scratch-banner ()
  "Generate a C64-style startup message for the *scratch* buffer."
  (let* ((emacs-version (format "GNU EMACS %s" emacs-version))
         (total-mem (/ (string-to-number
                        (shell-command-to-string "sysctl -n hw.memsize")) 1024))
         (page-size (string-to-number (shell-command-to-string "vm_stat | awk '/page size of/ {print $8}'")))
         (free-mem (* (string-to-number
                         (shell-command-to-string "vm_stat | awk '/Pages free/ {print $3}' | tr -d '.'"))
                        page-size))
         (mem-str (format "%dK RAM SYSTEM  %d BASIC BYTES FREE"
                          total-mem
                          free-mem)))
    (concat ";;;         **** " emacs-version " BASIC V2 ****\n"
            ";;;  " mem-str "\n"
            ";;;\n"
            ";;; READY.\n")))

(setq initial-scratch-message (format ";; Welcome to
;;
;;  ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;;  ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;;  █████╗  ██╔████╔██║███████║██║     ███████╗
;;  ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;;  ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;;  ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
;;
;;      Loading time : %s
;;      Packages     : %d
;;

"
                                      (emacs-init-time)
                                      (length package-activated-list)))



;; (setq initial-scratch-message "# This buffer is for notes you don't want to save\n\n")

;; for mac-style umlaut input
(setq mac-right-option-modifier nil)

;; create TAGS file in .cache dir
(setq tags-table-list '("~/.emacs.d/.cache"))

;; follow compilation buffer
(setq compilation-scroll-output 1)

;; Use San Francisco as font. Either copy it out of the Terminal app
;; and install it with FontBook or get it from here:
;; https://developer.apple.com/fonts/
(defun rb/set-face-attrs ()
  "Set fonts."
  (set-face-attribute 'default nil :font "SF Mono Light" :height 120)
  (set-face-attribute 'fixed-pitch nil :font "SF Mono Light" :height 120)
  (set-face-attribute 'variable-pitch nil :font "SF Mono Light" :height 120))

(defun rb/set-frame-size (frame)
  "Compute size and position for FRAME."
  (let* ((workarea (frame-monitor-workarea frame))
         (width (caddr workarea))
         (height (cadddr workarea)))

    ;; set initial size of frame
    (set-frame-size frame 99 60)

    ;; position frame slightly off center
    (set-frame-position frame
                        (* (/ width 100) 50)
                        (/ (- height (frame-pixel-height frame)) 2))))

;; Set frame size depending on whether we are a normal or daemon Emacs
(when (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (when (display-graphic-p frame)
                  (rb/set-face-attrs)
                  (rb/set-frame-size frame)))))

(unless (daemonp)
  (rb/set-face-attrs)
  (rb/set-frame-size (selected-frame)))


;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; load theme after custom file so that changes are trusted
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'wombat)

;; Enable hiding of minor modes from mode line
(use-package diminish)

(use-package swiper)

;; set up Ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper))
  :init
  (ivy-mode)
  :custom
  ;; (ivy-mode 1)
  (ivy-use-selectable-prompt t))

;; Project management using Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap ("s-p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-create-missing-test-files t))

(use-package ripgrep)

;; completion
(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package company
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection)
        ("<escape>" . company-abort))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.2)
  (company-tooltip-limit-20))

;; It's Magit!
(use-package magit
  :config
  (transient-append-suffix 'magit-push "-u"
                           '(1 "-o" "Skip CI pipeline" "-o ci.skip")))

(use-package forge
  :after magit
  :hook (forge-post-mode . (lambda () (auto-fill-mode -1))))

;; Add icons to mode line. After this package got installed, you need
;; to run the following command manually: M-x nerd-icons-install-fonts
(use-package nerd-icons)

;; Have a nice mode line
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 24)
  (doom-modeline-buffer-file-name-style 'file-name-with-project)
  (doom-modeline-position-column-line-format nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-enable-buffer-pos t)
  (doom-modeline-percent-position nil))

;; show all available completions of a shortcuts
;; helpful but can also be annoying
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode)

;; enable color-coded output in compilation buffer
(use-package ansi-color
  :ensure t
  :hook (compilation-filter . ansi-color-compilation-filter))

;; Org mode
(defun rb/init-org-mode ()
  "Configure org mode."
  (org-indent-mode)
  (auto-fill-mode t))

(defun rb/org-archive-location ()
  "Return location for archived tasks.
The string returns the filename where to store archived tasks. It
  contains the year and the month, for example
  `archive-2023-12.org`. The headline also contains month and year."
  (concat "archive-"
          (format-time-string "%Y-%m" (current-time))
          ".org::* Archived Tasks "
          (format-time-string "%B %Y" (current-time))))

(defun rb/delete-backward-to-slash ()
  "Delete"
  (interactive)
  (let ((slash-pos (save-excursion
                     (when (search-backward "/" nil t)
                       (point)))))
    (if slash-pos
        (delete-region (+ slash-pos 1) (point))
      (message "No / found before point."))))

(use-package org
  :hook (org-mode . rb/init-org-mode)
  :bind ("C-c c" . org-capture)
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (setq org-ellipsis " ↓")
  (setq org-todo-keywords
        '((sequence "TODO" "DOING" "|" "DONE")))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-archive-location (rb/org-archive-location))
  (setq org-agenda-files
        '("~/devel/agenda"))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/devel/agenda/tasks.org" "Tasks")
           "* TODO %?\n %U")
          ("r" "Reddit" plain (file "~/devel/org-roam/20250301205444-reddit_articles_for_upvoting.org")
           "\n\n%?\n"
           :empty-lines 1
           :before-finalize rb/delete-backward-to-slash)
          ("l" "Reading list" plain (file "~/devel/org-roam/20250413125520-reading_list.org")
           "- [ ] [[%^{url}][%^{desc}]]")
          )))

(global-set-key (kbd "C-c l") 'org-todo-list)

(use-package org-tempo
  :ensure nil
  :demand t)

;; org-roam
(use-package org-roam
  :custom
  (org-roam-directory "~/devel/org-roam")
  (org-roam-complete-everywhere t)
  (org-roam-dailies-directory "journal/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %? (%<%R>)"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: Journal Entry for %<%a, %d %b %Y>"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"   . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

;; verb for org-mode
(use-package verb)

;; Programming languages

(use-package flycheck
  :config (global-flycheck-mode)
  :bind(:map flycheck-mode-map
              ("C-c ! l" . (lambda ()
                             (interactive)
                             (flycheck-list-errors)
                             (switch-to-buffer-other-window  "*Flycheck errors*")))))

(flycheck-define-checker python-ruff
  "Python syntyax and style checker using ruff."
  :command ("ruff" "check" "--output-format" "json" source)
  :error-parser flycheck-parse-checkstyle
  :modes (python-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  ((lsp . lsp-lens-mode)
   (lsp-mode . lsp-enable-which-key-integration))
  :preface (setq lsp-use-plists t)
  :custom
  (lsp-modeline-code-actions-enable nil)
  (lsp-eldoc-enable-hover t)
  (lsp-signature-auto-activate t)
  (lsp-signature-render-documentation t)
  ;; (lsp-completion-enable-additional-text-edit nil)
  (read-process-output-max (* 1024 1024 8)))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

;; search for workspace symbols
(use-package lsp-ivy
  :after lsp-mode)

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :init
  (treemacs-resize-icons 14)
  :custom
  (lsp-treemacs-error-list-current-project-only t))

(use-package lsp-java
  :hook (java-mode . lsp-deferred)
  :bind (:map lsp-mode-map
              ("s-l g y" . lsp-java-type-hierarchy)
              ("s-1" . lsp-execute-code-action)
              ("s-l t m" . dap-java-run-test-method)
              ("s-l t c" . dap-java-run-test-class)
              ("s-l t d" . dap-java-debug-test-method))
  :custom
  (tab-width 4)
  :init
  (setq lsp-java-vmargs (list
                        "-XX:+UseG1GC"
                        "-XX:+UseStringDeduplication"))
  (which-key-add-key-based-replacements
    "s-l g y" "type hierarchy"
    "s-l t" "tests"
    "s-l t m" "run test method"
    "s-l t c" "run test class"
    "s-l t d" "debug test method")

  :config
  (setq c-basic-offset 4
        lsp-java-format-settings-url (concat "file://" (file-truename (locate-user-emacs-file "eclipse-formatter.xml"))))
  (setq lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.50.0/jdt-language-server-1.50.0-202509041425.tar.gz")
  (setq lsp-java-completion-favorite-static-members
        (vconcat lsp-java-completion-favorite-static-members
                 '("org.mockito.BDDMockito.*"
                   "org.hamcrest.MatcherAssert.*"
                   "org.hamcrest.Matchers.*")))
  )

(use-package dap-mode
  :after lsp-mode)

(use-package dap-java
  :ensure nil)

(use-package go-mode
  :hook ((go-mode . lsp-deferred)
         (go-mode . (lambda ()
                      (add-hook 'before-save-hook #'lsp-format-buffer nil t)
                      (add-hook 'before-save-hook #'lsp-organize-imports nil t)
                      (add-to-list 'company-backends '(company-capf :with company-yasnippet))
                      )))
  :custom (tab-width 4))


(use-package markdown-mode
  :mode ("\\.md\\'" "\\.apib\\'")
  :hook ((markdown-mode . display-line-numbers-mode)
         (markdown-mode . auto-fill-mode)))

(use-package yaml-mode
  :hook (yaml-mode . display-line-numbers-mode))

(use-package typescript-mode
  :hook (typescript-mode . lsp-deferred)
  :custom (typescript-indent-level 2))

(use-package js
  :ensure nil
  :custom (js-indent-level 2))

(use-package dockerfile-mode)

(use-package conf-mode
  :ensure nil
  :mode ("\\.env\\'" . conf-unix-mode))

(use-package groovy-mode
  :mode "\\.gradle\\'")

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         ;; (setq lsp-disabled-clients '(ruff))
                         (lsp-deferred)))
  :custom
  (lsp-pyright-langserver-command "pyright")
  (lsp-pyright-python-executable-cmd "python3")
  (lsp-pyright-log-level "trace")
  )

;; (use-package ripgrep)

(use-package python-mode
  :ensure nil
  :hook (python-mode . lsp-deferred))



;; ;; show inline git history
;; (use-package sideline-blame
;;   :init
;;   (setq sideline-backends-right '((sideline-blame . up)))
;;   :custom
;;   (sideline-blame-author-format "%s, ")
;;   (sideline-blame-datetime-format "%Y-%m-%d %H:%M ")
;;   (sideline-blame-commit-format "• %s"))


(use-package nxml-mode
  :ensure nil
  :hook ((nxml-mode . lsp-deferred)
         (nxml-mode . display-line-numbers-mode))
  :config
  (setq nxml-attribute-indent 4
        nxml-child-indent 4))


;; Restclient
(use-package restclient
  :mode ("\\.http$" . restclient-mode))

;; Custom functions and key bindings

;; comment everything
(global-set-key (kbd "s-/") 'comment-line)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; delete until beginning of line
(defun rb/delete-to-bol (delete-newline)
  "Delete current line and preceding newline char if DELETE-NEWLINE is set."
  (interactive "p")
  (delete-region (pos-bol) (pos-eol))
  (when delete-newline
    (delete-char -1)))

(global-set-key (kbd "s-<backspace>") (lambda () (interactive) (rb/delete-to-bol t)))
(global-set-key (kbd "s-S-<backspace>") (lambda () (interactive) (rb/delete-to-bol nil)))

;; get Eclipse like line moving
(defun rb/move-line-up ()
  "Move current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(global-set-key (kbd "M-<up>") 'rb/move-line-up)

(defun rb/move-line-down ()
  "Move current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "M-<down>") 'rb/move-line-down)

;; get Eclipse like line duplication
(defun rb/duplicate-line ()
  "Duplicate current line and append under the current one."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

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

(defvar rb-jump-point nil "Stores last jump point")
(defun rb/toggle-jump-point ()
  "Toggle between current location and last jump point."
  (interactive)
  (if rb-jump-point
      (let ((tmp (point)))
        (goto-char rb-jump-point)
        (setq rb-jump-point tmp))
    (setq rb-jump-point (point))))
(global-set-key (kbd "C-c SPC") 'rb/toggle-jump-point)

;; JWTs
(defun rb/decode-jwt ()
  "Decode JWT that is on the current line."
  (interactive)
  (let* ((data (split-string (thing-at-point 'filename) "\\."))
         (header (car data))
         (claims (cadr data)))
    (with-temp-buffer
      (insert (format "%s\n\n%s"
                      (base64-decode-string header t)
                      (base64-decode-string claims t)))
      (json-pretty-print-buffer)
      (with-output-to-temp-buffer "*JWT*"
        (special-mode)
        (princ (buffer-string))))) t)

(global-set-key (kbd "C-c j") 'rb/decode-jwt)

(defun rb/generate-uuid ()
  "Insert a generated UUID at point."
  (interactive)
  (insert (downcase (string-trim (shell-command-to-string "uuidgen")))))

(defun rb/projectile-kill-other-buffers ()
  "Kill all buffers in current project except for current buffer."
  (interactive)
  (let* ((current-buffer (current-buffer))
         (buffers (projectile-project-buffers)))
    (dolist (buffer buffers)
      (unless (eq buffer current-buffer)
        (kill-buffer buffer)))
    (message "Kill all buffers in project %s except %s"
             (projectile-project-name)
             (buffer-name current-buffer))))

(defun rb/delete-current-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file"
                              filename
                              "?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer"))))

(defun rb/add-word-at-point-to-dictionary ()
  "Add word at point to dictionary without asking."
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))

    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location
                           (cadr word) (caddr word) current-location))))
(global-set-key (kbd "C-c $") 'rb/add-word-at-point-to-dictionary)


(defun rb/toggle-true-false ()
  "Toggle the word at point between `true' and `false'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word))
        (case-fold-search nil)) ;; Make search case-sensitive
    (when bounds
      (let* ((start (car bounds))
             (end (cdr bounds))
             (word (buffer-substring-no-properties start end)))
        (cond
         ((string-equal word "true") (delete-region start end) (insert "false"))
         ((string-equal word "false") (delete-region start end) (insert "true")))))))
(global-set-key (kbd "C-c f") 'rb/toggle-true-false)

(defun rb/unix-timestamp-to-iso8601 ()
  "Convert Unix timestamp at point to an ISO8601 formatted date and display in minibuffer."
  (interactive)
  (let* ((timestamp (thing-at-point 'number t))
         (time (when timestamp (seconds-to-time timestamp))))
    (if time
        (message "ISO 8601 Date: %s" (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t))
      (message "No valid Unix timestamp found at point."))))
(global-set-key (kbd "C-c t") 'rb/unix-timestamp-to-iso8601)

(defun backward-kill-to-space ()
  "Delete backward until encountering whitespace or punctuation, like Alt+Del in other editors."
  (interactive)
  (let ((end (point)))
    ;; Skip any whitespace/punctuation first
    (skip-chars-backward "^[:alnum:]_")
    ;; Then skip the word characters
    (skip-chars-backward "[:alnum:]_")
    (delete-region (point) end)))
(global-set-key (kbd "M-DEL") 'backward-kill-to-space)

;;; init.el ends here
