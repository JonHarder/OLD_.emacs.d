;;; early-init.el --- Summary  -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:
(defvar byte-compile-warnings nil)

(setq package-enable-at-startup nil
      straight-use-package-by-default t)
;;; bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(menu-bar-mode -1)
(set-scroll-bar-mode nil)
(tool-bar-mode -1)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq initial-scratch-message nil)

(setq native-comp-async-report-warnings-errors 'silent)

;;;; some startup optimizations
(setq initial-major-mode 'fundamental-mode)
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

(add-to-list 'load-path (expand-file-name "ext_lisp" user-emacs-directory))

(provide 'early-init)
;;; early-init.el ends here
