(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil)))
  :custom
  (rust-format-on-save t))
(provide 'jh-rust)
;;; jh-rust ends here
