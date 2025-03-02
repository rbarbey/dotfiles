(setenv "LSP_USE_PLISTS" "true")

;; Temporarily increase gc during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Restore to normal value after startup
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
