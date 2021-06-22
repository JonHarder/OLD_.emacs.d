(defun modules/rust--load (config)
  (setq rust-format-on-save t)
  (use-package rust-mode
    :config
    (add-hook 'rust-mode-hook
              (lambda () (setq indent-tabs-mode nil)))))
