;;; early-init.el --- Summary  -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:
(defvar byte-compile-warnings nil)

(setq package-quickstart t
      package-enable-at-startup nil
      package--init-file-ensured t)

(require 'package)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(menu-bar-mode -1)
(set-scroll-bar-mode nil)
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
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(when (eq system-type 'darwin)
  (setq-default
   mac-option-modifier nil
   mac-command-modifier 'meta
   mac-redisplay-dont-reset-vscroll t
   mac-mouse-wheel-smooth-scroll nil))

(defun jh/post-init-hook ()
  "Post initialization function."
  (setq file-name-handler-alist old-file-name-handler))

(add-hook 'after-init-hook #'jh/post-init-hook)

(provide 'early-init)
;;; early-init.el ends here
