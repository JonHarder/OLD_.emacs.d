;;; early-init.el --- Summary  -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:
(defvar byte-compile-warnings nil)

(setq package-enable-at-startup t)
(defvar package-quickstart)

(setq package-quickstart t)

(menu-bar-mode -1)
(set-scroll-bar-mode nil)
;; (toggle-scroll-bar -1)
(tool-bar-mode -1)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)

(setq native-comp-async-report-warnings-errors 'silent)

(setq frame-inhibit-implied-resize t)
(setq fast-but-imprecise-scrolling t)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq-default bidi-display-reordering 'left-to-right)
(setq idle-update-delay 1)
(setq auto-mode-case-fold nil)

(defvar old-file-name-handler file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(defun jh/post-init-hook ()
  "Post initialization function."
  (setq gc-cons-threshold (* 2 1000 1000)
        gc-cons-percentage 0.1
        file-name-handler-alist old-file-name-handler))

(add-hook 'after-init-hook #'jh/post-init-hook)

(provide 'early-init)
;;; early-init.el ends here
