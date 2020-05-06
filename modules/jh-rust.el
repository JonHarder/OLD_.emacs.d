;;; rust --- Summary
;; configure settings for rust development

;;; Commentary:

;;; Code:

(defun modules/rust--load (config)
  (setq rust-format-on-save t)
  (use-package rust-mode
    :defer 4
    :config
    (add-hook 'rust-mode-hook
              (lambda () (setq indent-tabs-mode nil)))))


(provide 'jh-rust)
;;; jh-rust.el ends here
