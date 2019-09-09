;;; modeline --- Summary

;;; Commentary:

;;; Code:

(defun modules/modeline--load (config)
  "Load configuration for the modeline using CONFIG."
  (use-package doom-modeline
    :init
    (setq doom-modeline-major-mode-color-icon t)
    :hook (after-init . doom-modeline-mode)))

(provide 'jh-modeline)
;;; jh-modeline.el ends here
