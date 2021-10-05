;;; lisp --- Configuration common to lisp style languages

;;; Commentary:
;;; Specific configuration for a particular language can be found
;;; in a module with it's name


;;; Code:
(use-package eros
  :hook (lisp-data-mode . eros-mode)
  :general
  (:keymaps 'emacs-lisp-mode-map
   :states 'normal
   :prefix ","
   "e" 'eros-eval-last-sexp))

(add-hook 'emacs-lisp-mode-hook #'show-paren-mode)

(setq inferior-lisp-program "/usr/local/bin/sbcl")

(use-package parinfer-rust-mode
  :disabled t
  :hook lisp-data-mode
  :init
  (setq parinfer-rust-auto-download t)
  :config
  (add-to-list 'parinfer-rust-treat-command-as '(evil-open-below . "paren"))
  (add-to-list 'parinfer-rust-treat-command-as '(evil-open-above . "paren")))

(use-package symex
  :config
  (symex-initialize)
  :general
  (:keymaps 'global
   :states 'normal
   "S" #'symex-mode-interface))

(provide 'lisp)
;;; lisp.el ends here
