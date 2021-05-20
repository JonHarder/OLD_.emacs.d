;;; lisp --- Configuration common to lisp style languages

;;; Commentary:
;;; Specific configuration for a particular language can be found
;;; in a module with it's name


;;; Code:
(require 'use-package)
(defun modules/lisp--load (config)
  "Configure Lisp modes, as determined by `CONFIG'."
  (use-package sly
    :init
    (setq inferior-lisp-program "/usr/local/bin/sbcl"))

  (use-package eros
    ;;; display inline overlays for elisp evaluations
    :init
    (eros-mode 1))

  (use-package parinfer-rust-mode
    :hook emacs-lisp-mode
    :init
    (setq parinfer-rust-auto-download t)
    :config
    (add-to-list 'parinfer-rust-treat-command-as '(evil-open-below . "paren"))
    (add-to-list 'parinfer-rust-treat-command-as '(evil-open-above . "paren"))))

(provide 'lisp)
;;; lisp.el ends here
