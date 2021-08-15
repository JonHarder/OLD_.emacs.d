;;; projects --- Generic Project management configuration

;;; Commentary:


;;; Code:
(use-package projectile
  :custom
  (projectile-create-missing-test-files t)
  :config
  (projectile-mode 1)
  :general
  (:states 'normal
   :prefix "SPC"
   "p" '(:ignore t :wk "Project")
   "p d" 'projectile-dired
   "p s" 'projectile-run-eshell
   "p p" '(projectile-switch-project :wk "Switch Project")
   "p f" '(projectile-find-file :wk "Find File in Project")
   "p b" 'projectile-ibuffer
   "p t" '(projectile-toggle-between-implementation-and-test :wk "Toggle With Test")))

(provide 'projects)
;;; projects.el ends here
