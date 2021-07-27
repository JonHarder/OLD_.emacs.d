;;; projects --- Generic Project management configuration

;;; Commentary:


;;; Code:
(require 'use-package)
(require 'general)

(defun modules/projects--load (config)
  "Load project based configuration using CONFIG."
  (use-package projectile
    :ensure t
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
     "p /" '(rg :wk "RipGrep")
     "p t" '(projectile-toggle-between-implementation-and-test :wk "Toggle With Test"))))

(provide 'projects)
;;; projects.el ends here
