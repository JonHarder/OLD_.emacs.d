;;; dired --- Configuration for the built in dired file manager -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:
;; (require 'contrib "~/.emacs.d/contrib.el")
;; (require 'general)

;; (eval-after-load "dired"
;;   '(require 'dired-x))

(use-package diredfl
  :config
  (diredfl-global-mode 1))

(put 'dired-find-alternate-file 'disabled nil)

 ;; (use-package sunrise
 ;;  :quelpa sunrise-commander)

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
  (:states 'normal
   :keymaps 'dired-mode-map
   "+" 'dired-create-directory
   "h" 'dired-up-directory
   "u" 'dired-unmark
   "f" 'find-file
   "l" 'dired-find-file
   "C" 'dired-do-copy))
  
(use-package dired-rainbow
  :after dired)

(use-package dired-hide-dotfiles
  :after dired
  :general
  (:keymaps 'dired-mode-map
   :states 'normal
   "H" 'dired-hide-dotfiles-mode))

(use-package dired-subtree
  :general
  (:keymaps 'dired-mode-map
   :states 'normal
   "TAB" 'dired-subtree-toggle))

(provide 'dired)
;;; dired.el ends here
