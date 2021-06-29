;;; git --- Git (But really just magit) configuration

;;; Commentary:
;;; magit is amazing.  That is all.

;;; Code:
(require 'use-package)

(defun modules/git--load (config)
  "Load configuration for working with git using CONFIG."
  (use-package magit-section)
  (use-package magit
    :config
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))
    (defun magit-quite-session ()
      "Restores the previous window configuration and kills the magit buffer."
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen))
    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)))

(provide 'git)
;;; git.el ends here
