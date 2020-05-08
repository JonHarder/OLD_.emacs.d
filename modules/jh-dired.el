;;; dired --- Summary

;;; Commentary:

;;; Code:
(defun modules/dired--load (config)
  "Load configuration for dired, using CONFIG."
  (straight-use-package 'ranger)
  (straight-use-package 'diredfl)
  (put 'dired-find-alternate-file 'disabled nil)

  (use-package dired-narrow
    :config
    (general-define-key
     :states 'normal
     :keymaps 'dired-mode-map
     "/" 'dired-narrow))

  (use-package dired-subtree
    :defer 2
    :bind (:map dired-mode-map
            ("<tab>" . 'dired-subtree-toggle)
            ("<c-tab>" . 'dired-subtree-cycle))))

(provide 'jh-dired)
;;; jh-dired.el ends here
