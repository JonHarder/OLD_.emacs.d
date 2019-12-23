;;; lisp --- Summary


;;; Commentary:

;;; Code:
(defun modules/lisp--load (config)
  (use-package slime
    :straight t
    :init
    (setq inferior-lisp-program "/usr/bin/sbcl"
          slime-contribs '(slime-fancy)))

  (use-package parinfer
    :init
    (progn
      (setq parinfer-extensions
            '(defaults
              pretty-parens
              evil
              smart-tab))
      (add-hook 'clojure-mode-hook #'parinfer-mode)
      (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
      (add-hook 'common-lisp-mode-hook #'parinfer-mode)
      (add-hook 'scheme-mode-hook #'parinfer-mode)
      (add-hook 'lisp-mode-hook #'parinfer-mode))))


(provide 'jh-clojure)
;;; jh-clojure.el ends here
