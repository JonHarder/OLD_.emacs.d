;;; package --- Go config -*- lexical-binding: t -*-

;;; Commentary:
;;; Configure the environment for Go programming.


;;; Code:
(defun jh/go-mode-hook ()
  (add-hook 'before-save-hook 'lsp-format-buffer t t)
  (add-hook 'before-save-hook 'lsp-organize-imports t t))

(use-package lsp-ui
  :after go
  :hook (go-mode-hook 'lsp-ui-mode))

(use-package go-mode
  :mode "\\.go\\'"
  :config
  (add-hook 'go-mode-hook 'jh/go-mode-hook))


(provide 'go)
;;; go.el ends here
