;;; lisp --- Configuration common to lisp style languages

;;; Commentary:
;;; Specific configuration for a particular language can be found
;;; in a module with it's name


;;; Code:
(use-package eros
  :hook (lisp-data-mode . eros-mode))

(add-hook 'emacs-lisp-mode-hook #'show-paren-mode)

(use-package parinfer-rust-mode
  :hook lisp-data-mode
  :init
  (setq parinfer-rust-auto-download t)
  :config
  (add-to-list 'parinfer-rust-treat-command-as '(evil-open-below . "paren"))
  (add-to-list 'parinfer-rust-treat-command-as '(evil-open-above . "paren")))

(provide 'lisp)
;;; lisp.el ends here
