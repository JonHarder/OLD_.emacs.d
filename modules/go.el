(defun modules/go--load (config)
  (defun jh/go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save))
  (use-package go-mode
    :config
    (add-hook 'go-mode-hook 'jh/go-mode-hook)))
