;;; dired --- Configuration for the built in dired file manager -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:
(require 'contrib "~/.emacs.d/contrib.el")
(require 'general)

(defun modules/dired--load (config)
  "Load configuration for dired, using CONFIG."
  (use-package diredfl
    :ensure t
    :config
    (diredfl-global-mode 1))
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "-lahoG")

  ;; (use-package sunrise
  ;;  :quelpa sunrise-commander)

  (use-package dired
    :commands (dired dired-jump)
    :after general
    :general
    (:keymaps 'dired-mode-map
     :states 'normal
     "s" 'eshell
     "f" 'find-file
     "o" 'ace-window
     "l" 'dired-find-file
     "h" 'dired-up-directory
     "RET" 'dired-find-file-other-window
     "q" (lambda () (interactive) (quit-window t)))
    (:states 'normal
     :prefix "SPC"
     "a d" '(dired-jump :wk "Dired")))

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
     "TAB" 'dired-subtree-toggle)))

(provide 'dired)
;;; dired.el ends here
