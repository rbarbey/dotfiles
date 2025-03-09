;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Some early init like garbage collection and compilation config.
;;

;;; Code:

;; Enable plists for LSP packages
(setenv "LSP_USE_PLISTS" "true")

;; Temporarily increase gc during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Restore to normal value after startup
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

;; Some UI settings so that there's not flickering before the rest of
;; the UI is initialized
(setq default-frame-alist '((menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (background-color . "#242424")))

;;; early-init.el ends here
