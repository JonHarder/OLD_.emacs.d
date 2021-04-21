(defun modules/python--load (config)
  "Python configuration using CONFIG."
  (use-package flycheck-pycheckers
    :defer 8
    :config
    (with-eval-after-load 'flycheck
      (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))

  (straight-use-package 'pyvenv)

  (add-hook 'python-mode-hook #'lsp))
