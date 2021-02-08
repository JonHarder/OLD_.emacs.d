(defun modules/lisp--load (config)
  (use-package sly
    :init
    (setq inferior-lisp-program "/usr/local/bin/sbcl"))

  (use-package eros
    ;;; display inline overlays for elisp evaluations
    :init
    (eros-mode 1))


  (use-package parinfer
    :demand t
    :init
    (progn
      (setq parinfer-extensions
            '(defaults
               pretty-parens
               evil
               smart-tab
               smart-yank))
      (add-hook 'clojure-mode-hook #'parinfer-mode)
      (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
      (add-hook 'common-lisp-mode-hook #'parinfer-mode)
      (add-hook 'scheme-mode-hook #'parinfer-mode)
      (add-hook 'lisp-mode-hook #'parinfer-mode))))
