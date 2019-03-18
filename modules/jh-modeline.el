;;; modeline --- Summary

;;; Commentary:

;;; Code:
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-major-mode-color-icon t)
  :hook (after-init . doom-modeline-mode))

(provide 'jh-modeline)
;;; jh-modeline.el ends here
