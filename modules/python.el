;;; python --- Configuration for developing using python

;;; Commentary:

;;; Code:
(require 'straight)
(require 'use-package)
(require 'lsp)

(defun modules/python--load (config)
  "Python configuration using CONFIG."
  ;; (use-package flycheck-pycheckers
  ;;   :config
  ;;   (with-eval-after-load 'flycheck
  ;;     (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))
  (straight-use-package 'pyvenv)
  (add-hook 'python-mode-hook #'lsp))
(provide 'python.el)
;;; python.el ends here
