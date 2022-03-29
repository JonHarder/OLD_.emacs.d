;;; package --- Go config -*- lexical-binding: t -*-

;;; Commentary:
;;; Configure the environment for Go programming.


;;; Code:
(require 'use-package)

(defun jh/go-mode-hook ()
  "Personal hook to be ran when opening go files."
  (add-hook 'before-save-hook 'lsp-format-buffer t t)
  (add-hook 'before-save-hook 'lsp-organize-imports t t))

(use-package go-mode
  :mode "\\.go\\'"
  :config
  (add-hook 'go-mode-hook 'jh/go-mode-hook))


(provide 'jh-go)
;;; jh-go.el ends here
