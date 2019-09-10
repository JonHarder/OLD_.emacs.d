;;; dired --- Summary

;;; Commentary:

;;; Code:
(defun modules/dired--load (config)
  "Load configuration for dired, using CONFIG."
  (straight-use-package 'ranger))

(provide 'jh-dired)
;;; jh-dired.el ends here
