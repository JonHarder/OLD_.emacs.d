;;; init --- Summary
;; my emacs configuration

;;; Commentary:
;; bootstrap the package system, configure the module loader
;; load modules

;;; Code:


(defun modules/python--load (config)
  (use-package flycheck-pycheckers
    :straight t
    :config
    (with-eval-after-load 'flycheck
      (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))

  (add-to-list 'auto-mode-alist '("\\Pipfile\\'" . conf-mode))

  (use-package elpy
    :straight t
    :init
    (elpy-enable))
  
  (use-package pipenv
    :straight t
    :hook (python-mode . pipenv-mode)
    :init
    (setq
     pipenv-projectile-after-switch-function
     #'pipenv-projectile-after-switch-extended)))

(provide 'jh-python)
;;; jh-python.el ends here
