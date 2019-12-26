;;; dired --- Summary

;;; Commentary:

;;; Code:
(defun modules/dired--load (config)
  "Load configuration for dired, using CONFIG."
  (straight-use-package 'ranger)
  (straight-use-package 'diredfl)
  (use-package dired-subtree
    :bind (:map dired-mode-map
            ("<tab>" . 'dired-subtree-toggle)
            ("<c-tab>" . 'dired-subtree-cycle))))

(provide 'jh-dired)
;;; jh-dired.el ends here
