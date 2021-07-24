(require 'use-package)
(defun modules/rust--load (config)
  "Load rust configuration using CONFIG."
  (use-package rust-mode
    :ensure t
    :mode "\\.rs\\'"
    :config
    (add-hook 'rust-mode-hook
              (lambda () (setq indent-tabs-mode nil)))
    (rust-format-on-save t)))
