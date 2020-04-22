;;; rust --- Summary
;; configure settings for rust development

;;; Commentary:

;;; Code:

(defun modules/rust--load (config)
  (setq rust-format-on-save t)
  (straight-use-package 'rust-mode)
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil))))


(provide 'jh-rust)
;;; jh-rust.el ends here
