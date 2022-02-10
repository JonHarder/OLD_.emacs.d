;;; dired --- Configuration for the built in dired file manager -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:
;; (require 'contrib "~/.emacs.d/contrib.el")
;; (require 'general)

;; (eval-after-load "dired"
;;   '(require 'dired-x))
(use-package ranger)

(use-package diredfl
  :after dired
  :config
  (diredfl-global-mode 1))

(put 'dired-find-alternate-file 'disabled nil)

(defun dired-home ()
  "Jumps dired window to the home directory."
  (interactive nil dired-mode)
  (dired "~/"))

(defun dired-here ()
  "Open `dired' in the directory of the current buffer."
  (interactive)
  (dired default-directory))

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :custom
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-deletion-confirmer 'y-or-n-p)
  (dired-do-revert-buffer t)
  (dired-dwim-target t)
  (dired-listing-switches "-lvahoG")
  :general
  (:states 'normal
   :prefix "SPC"
   "a d" 'dired-here)
  (:keymaps 'dired-mode-map
   ;; :states '(emacs normal motion)
   :states '(normal motion)
   "RET" 'dired-find-alternate-file
   "(" 'dired-hide-details-mode
   "j" 'dired-next-line
   "k" 'dired-previous-line
   "/" 'dired-goto-file
   "+" 'dired-create-directory
   "x" 'dired-do-flagged-delete
   "~" 'dired-home
   "d" 'dired-flag-file-deletion
   "h" 'dired-up-directory
   "u" 'dired-unmark
   "o" 'dired-find-file-other-window
   "m" 'dired-mark
   "g r" 'revert-buffer
   "v" 'dired-view-file
   "t" 'dired-toggle-marks
   "-" 'dired-do-kill-lines
   "s" 'dired-sort-toggle-or-edit
   "f" 'find-file
   "l" 'dired-find-alternate-file
   "C" 'dired-do-copy
   "S" 'dired-mark-suffix
   "D" 'dired-do-delete
   "R" 'dired-do-rename))

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
