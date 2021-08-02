;;; dired --- Configuration for the built in dired file manager -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:
(require 'contrib "~/.emacs.d/contrib.el")
(require 'general)

(eval-after-load "dired"
  '(require 'dired-x))

(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode 1))
(put 'dired-find-alternate-file 'disabled nil)

 ;; (use-package sunrise
 ;;  :quelpa sunrise-commander)

(use-package dired
  :commands (dired dired-jump)
  :after general
  :custom
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-deletion-confirmer 'y-or-n-p)
  (dired-dwim-target t)
  (dired-listing-switches "-lvahoG")
  :general
  (:keymaps 'dired-mode-map
   :states 'normal
   "s" 'eshell
   "f" 'find-file
   "l" 'dired-find-file
   "w" 'ace-window
   "h" 'dired-up-directory
   "v" 'dired-view-file
   "RET" 'dired-find-file-other-window
   "q" (lambda () (interactive) (quit-window t)))
  (:states 'normal
   :prefix "SPC"
   "a d" 'dired-jump))

(use-package dired-rainbow
  :ensure t
  :after dired)

(use-package dired-hide-dotfiles
  :ensure t
  :after dired
  :general
  (:keymaps 'dired-mode-map
   :states 'normal
   "H" 'dired-hide-dotfiles-mode))

(use-package dired-subtree
  :ensure t
  :general
  (:keymaps 'dired-mode-map
   :states 'normal
   "TAB" 'dired-subtree-toggle))

(provide 'dired)
;;; dired.el ends here
