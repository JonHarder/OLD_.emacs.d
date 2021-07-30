;;; git --- Git (But really just magit) configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; magit is amazing.  That is all.

;;; Code:
(require 'use-package)

(use-package magit-section
  :ensure t
  :after magit)

(use-package magit
  :ensure t
  :commands magit-status
  :config
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer."
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))
  :general
  (:states 'normal
   :prefix "SPC"
   "g" '(:ignore t :wk "Git")
   "g s" '(magit-status :wk "Status")
   "g l" '(magit-log :wk "Logs")
   "g c" '(magit-commit :wk "Commit")
   "g f" '(magit-file-dispatch :wk "File Dispatch")
   "g d" '(magit-dispatch :wk "Dispatch"))
  (:keymaps 'magit-status-mode-map
   :states 'normal
   "q" #'magit-quit-session)
  (:keymaps 'magit-refs-mode-map
   :states 'normal
   "x" #'magit-delete-thing))

(provide 'git)
;;; git.el ends here
