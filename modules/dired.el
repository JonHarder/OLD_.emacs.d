;;; dired --- Configuration for the built in dired file manager -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:
;; (require 'contrib "~/.emacs.d/contrib.el")
;; (require 'general)

;; (eval-after-load "dired"
;;   '(require 'dired-x))

(use-package diredfl
  :after dired
  :config
  (diredfl-global-mode 1))

(put 'dired-find-alternate-file 'disabled nil)

(defun dired-home ()
  "Jumps dired window to the home directory."
  (interactive nil dired-mode)
  (dired "~/"))

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :custom
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-deletion-confirmer 'y-or-n-p)
  (dired-dwim-target t)
  (dired-listing-switches "-lvahoG")
  :general
  (:states 'normal
   :prefix "SPC"
   "a d" 'dired-jump)
  (:keymaps 'dired-mode-map
   :states '(emacs normal motion)
   "RET" 'dired-find-file
   "+" 'dired-create-directory
   "x" 'dired-do-flagged-delete
   "~" 'dired-home
   "d" 'dired-flag-file-deletion
   "h" 'dired-up-directory
   "u" 'dired-unmark
   "o" 'dired-find-file-other-window
   "m" 'dired-mark
   "g r" 'revert-buffer
   "f" 'find-file
   "l" 'dired-find-file
   "C" 'dired-do-copy
   "R" 'dired-do-rename
   "D" 'dired-do-delete))

(use-package dired-x
  :straight nil
  :after dired
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-files
        (concat dired-omit-files "\\|.organice-bak$")))
  
(use-package dired-rainbow
  :after dired)

(use-package dired-hide-dotfiles
  :after dired
  :general
  (:keymaps 'dired-mode-map
   :states 'normal
   "." 'dired-hide-dotfiles-mode))

(use-package dired-subtree
  :general
  (:keymaps 'dired-mode-map
   :states 'normal
   "TAB" 'dired-subtree-toggle))

(provide 'dired)
;;; dired.el ends here
