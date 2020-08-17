(defun modules/git--load (config)
  "Load configuration for working with git using CONFIG."
  ;; (use-package git-gutter
  ;;   :config
  ;;   (global-git-gutter-mode +1))
  
  (use-package magit
    :defer 10))
