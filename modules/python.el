;;; python --- Configuration for developing using python

;;; Commentary:

;;; Code:
(require 'use-package)
(require 'lsp)

(defun modules/python--load (config)
  "Python configuration using CONFIG."
  (add-hook 'python-mode-hook #'lsp))
(provide 'python.el)
;;; python.el ends here
