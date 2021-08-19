;;; git --- Git (But really just magit) configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; magit is amazing.  That is all.

;;; Code:
(use-package magit-section
  :after magit)

(use-package magit
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
   "g s" 'magit-status
   "g l" 'magit-log
   "g c" 'magit-commit
   "g f" 'magit-file-dispatch
   "g d" 'magit-dispatch)
  (:keymaps 'magit-status-mode-map
   :states 'normal
   "?" #'magit-dispatch
   "b" #'magit-branch
   "q" #'magit-quit-session
   "j" #'magit-next-line
   "k" #'magit-previous-line
   "g r" #'magit-refresh
   "s" #'magit-stage
   "x" #'magit-discard
   "c" #'magit-commit
   "m" #'magit-merge
   "$" #'magit-process-buffer
   "f" #'magit-fetch
   "t" #'magit-tag
   "F" #'magit-pull
   "M" #'magit-remote
   "P" #'magit-push
   "RET" #'magit-diff-visit-file
   "TAB" #'magit-section-toggle)
  (:keymaps 'magit-refs-mode-map
   :states 'normal
   "x" #'magit-delete-thing))

(provide 'git)
;;; git.el ends here
