(defun modules/dired--load (config)
  "Load configuration for dired, using CONFIG."
  (straight-use-package 'diredfl)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "-lahoG")

  (use-package dired-single
    :config
    (evil-define-key 'normal dired-mode-map
      "h" 'dired-single-up-directory
      "l" 'dired-single-buffer
      (kbd "RET") 'dired-single-buffer))

  (evil-define-key 'normal dired-mode-map
    "s" 'eshell
    "n" 'edwina-select-next-window
    "p" 'edwina-select-previous-window
    "f" 'find-file)
  

  (use-package dired-collapse)
  (use-package dired-rainbow)

  (use-package dired-hide-dotfiles
    :hook (dired-mode . dired-hide-dotfiles-mode)
    :config
    (evil-define-key 'normal dired-mode-map
      "H" 'dired-hide-dotfiles-mode))

  (use-package dired-subtree
    :config
    (general-define-key
     :states 'normal
     :keymaps 'dired-mode-map
     "TAB" 'dired-subtree-toggle
     [?\C-\t] 'dired-subtree-cycle)))
