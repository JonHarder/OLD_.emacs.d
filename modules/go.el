(defun modules/go--load (config)
  (defun jh/go-mode-hook ()
    (add-hook 'before-save-hook 'lsp-format-buffer t t)
    (add-hook 'before-save-hook 'lsp-organize-imports t t))
  (use-package go-mode
    :config
    (add-hook 'go-mode-hook 'jh/go-mode-hook)))
