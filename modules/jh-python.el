;;; init --- Summary
;; my emacs configuration

;;; Commentary:
;; bootstrap the package system, configure the module loader
;; load modules

;;; Code:


(defun modules/python--load (config)
  "Python configuration using CONFIG."
  (use-package flycheck-pycheckers
    :defer 8
    :config
    (with-eval-after-load 'flycheck
      (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))

  (straight-use-package 'pyvenv)

  (add-hook 'python-mode-hook #'lsp)
  (add-to-list 'auto-mode-alist '("\\Pipfile\\'" . conf-mode)))


(provide 'jh-python)
;;; jh-python.el ends here
