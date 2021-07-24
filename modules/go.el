;;; package --- Go config -*- lexical-binding: t -*-

;;; Commentary:
;;; Configure the environment for Go programming.


;;; Code:
(require 'use-package)

(defun modules/go--load (config)
  "Configure environment for go programming with CONFIG."
  (defun jh/go-mode-hook ()
    (add-hook 'before-save-hook 'lsp-format-buffer t t)
    (add-hook 'before-save-hook 'lsp-organize-imports t t))
  (use-package go-mode
    :ensure t
    :mode "\\.go\\'"
    :config
    (add-hook 'go-mode-hook 'jh/go-mode-hook)))

(provide 'go)
;;; go.el ends here
