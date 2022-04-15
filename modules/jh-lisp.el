;;; lisp --- Configuration common to lisp style languages

;;; Commentary:
;;; Specific configuration for a particular language can be found
;;; in a module with it's name


;;; Code:
(require 'use-package)
(require 'general)

(use-package eros
  :after general
  :hook (lisp-data-mode . eros-mode)
  :general
  (:keymaps 'emacs-lisp-mode-map
   :states 'normal
   :prefix ","
   "e" #'eros-eval-last-sexp
   "RET" #'eros-eval-defun))

(add-hook 'emacs-lisp-mode-hook #'show-paren-mode)

(setq inferior-lisp-program
      (if MAC-M1-P
          "/opt/homebrew/bin/sbcl"
        "/usr/local/bin/sbcl"))
    

(use-package sly
  :after general
  :config
  (general-define-key
   :keymaps 'lisp-mode-map
   :states 'normal
   :prefix ","
   "RET" #'sly-compile-defun
   "e" #'sly-expand-1
   "f" #'sly-load-file
   "i" #'sly-inspect)
  (general-define-key
   :keymaps 'sly-db-mode-map
   :states 'normal
   "q" #'sly-db-abort
   "0" #'sly-db-invoke-restart-0
   "1" #'sly-db-invoke-restart-1
   "2" #'sly-db-invoke-restart-2
   "3" #'sly-db-invoke-restart-3
   "4" #'sly-db-invoke-restart-4
   "5" #'sly-db-invoke-restart-5
   "6" #'sly-db-invoke-restart-6
   "7" #'sly-db-invoke-restart-7
   "8" #'sly-db-invoke-restart-8
   "9" #'sly-db-invoke-restart-9)
  (general-define-key
   :keymaps 'sly-mrepl-mode-map
   :states 'normal
   :prefix ","
   "c" #'sly-mrepl-clear-repl))

(use-package parinfer-rust-mode
  :disabled t
  :hook lisp-data-mode
  :init
  (setq parinfer-rust-auto-download t)
  :config
  (parinfer-rust-mode-enable)
  (add-to-list 'parinfer-rust-treat-command-as '(evil-open-below . "paren"))
  (add-to-list 'parinfer-rust-treat-command-as '(evil-open-above . "paren")))

(use-package evil-lispy
  :hook ((emacs-lisp-mode . evil-lispy-mode)
         (lisp-mode . evil-lispy-mode)))

(general-define-key
 :keymaps 'lisp-mode-map
 :states 'normal
 :prefix ","
 "e" #'sly-expand-1)

(use-package symex
  :after general
  :config
  (symex-initialize)
  :general
  (:keymaps 'global
   :states 'normal
   "S" #'symex-mode-interface))

(provide 'jh-lisp)
;;; jh-lisp.el ends here
