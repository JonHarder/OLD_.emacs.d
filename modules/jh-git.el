;;; git --- Git (But really just magit) configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; magit is amazing.  That is all.

;;; Code:
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
  (evil-set-initial-state 'magit-log-mode 'normal)
  (evil-set-initial-state 'magit-revision-mode 'normal)
  :general
  (:states 'normal
   :prefix "SPC"
   "g s" 'magit-status
   "g l" 'magit-log
   "g c" 'magit-commit
   "g f" 'magit-file-dispatch
   "g d" 'magit-dispatch)
  (:keymaps 'magit-log-mode-map
   :states 'normal
   "j" #'magit-next-line
   "k" #'magit-previous-line
   "r" #'magit-rebase
   "?" #'magit-dispatch
   "RET" #'magit-show-commit)
  (:keymaps 'magit-revision-mode-map
   :states 'normal
   "RET" #'magit-diff-visit-file
   "TAB" #'magit-section-toggle
   "?" #'magit-dispatch)
  (:keymaps 'magit-status-mode-map
   :states 'normal
   "$" #'magit-process-buffer
   "?" #'magit-dispatch
   "g u" #'magit-jump-to-unstaged
   "g s" #'magit-jump-to-staged
   "g U" #'magit-jump-to-untracked
   "b" #'magit-branch
   "c" #'magit-commit
   "e" #'magit-ediff-dwim
   "f" #'magit-fetch
   "i" #'magit-gitignore
   "g r" #'magit-refresh
   "j" #'magit-section-forward
   "k" #'magit-section-backward
   "l" #'magit-log
   "m" #'magit-merge
   "q" #'magit-quit-session
   "s" #'magit-stage
   "t" #'magit-tag
   "x" #'magit-discard
   "z" #'magit-stash
   "F" #'magit-pull
   "M" #'magit-remote
   "P" #'magit-push
   "RET" #'magit-diff-visit-file
   "TAB" #'magit-section-toggle)
  (:keymaps 'magit-refs-mode-map
   :states 'normal
   "x" #'magit-delete-thing))

(use-package magit-section
  :after magit
  :general
  (:keymaps 'magit-section-mode-map
   :states 'normal
   "TAB" #'magit-section-toggle))

(use-package code-review
  :commands (code-review-start)
  :after magit
  :config
  (defun jh/org-code-review-block (url)
    ;;; BitBuckets api is incomplete, poorly documented and variously broken.
    ;;; therefore, this only supports github for now.
    (when (string-match-p "github" url)
      (format "#+begin_src elisp :results none\n(code-review-start \"%s\")\n#+end_src" url))))

(provide 'jh-git)
;;; jh-git.el ends here
