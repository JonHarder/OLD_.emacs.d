;;; core --- Summary
;; Core configuration setup

;;; Commentary:
;; This sets up the basics of the configuration including use-package,
;; and basic settings (or things that I can't find a better place to put them into)

;;; Code:
(defun modules/git--load (config)
  "Load configuration for working with git using CONFIG."
  ;; (use-package git-gutter
  ;;   :config
  ;;   (global-git-gutter-mode +1))
  
  (straight-use-package 'magit)
  (straight-use-package 'forge))

(provide 'jh-git)
;;; jh-git.el ends here
