;;; code --- Summary
;; General use settings/packages that are language agnostic

;;; Commentary:

;;; Code:

(defun modules/code--load (config)
  "Module definition for generic programming, configured by CONFIG."
  (straight-use-package 'dumb-jump)

  (defun jh/prog-mode-hook ()
    "Settings that should be enabled or disabled for all programming modes."
    (setq-default whitespace-style '(face tabs space-before-tab line-tail empty space-after-tab tab-mark))
    (hl-line-mode)
    (whitespace-mode 1))

  (add-hook 'prog-mode-hook #'jh/prog-mode-hook)

  (use-package editorconfig
    :defer 6
    :config
    (editorconfig-mode 1)))


(provide 'jh-code)
;;; jh-code.el ends here
