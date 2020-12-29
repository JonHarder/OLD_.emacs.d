(defun modules/lisp--load (config)
  (use-package slime
    :demand t
    :init
    (setq inferior-lisp-program "/usr/local/bin/sbcl"
          slime-contribs '(slime-fancy)))

  (use-package parinfer
    :demand t
    :init
    (progn
      (setq parinfer-extensions
            '(defaults
               pretty-parens
               evil
               paredit
               smart-tab
               smart-yank))
      (add-hook 'clojure-mode-hook #'parinfer-mode)
      (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
      (add-hook 'common-lisp-mode-hook #'parinfer-mode)
      (add-hook 'scheme-mode-hook #'parinfer-mode)
      (add-hook 'lisp-mode-hook #'parinfer-mode))))
