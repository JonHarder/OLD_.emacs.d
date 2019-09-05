;;; lisp --- Summary


;;; Commentary:

;;; Code:
(defun modules/lisp--load (config)
  (use-package slime
    :straight t
    :init
    (setq inferior-lisp-program "/usr/bin/sbcl"
          slime-contribs '(slime-fancy))))


(provide 'jh-clojure)
;;; jh-clojure.el ends here
