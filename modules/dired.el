(defun modules/dired--load (config)
  "Load configuration for dired, using CONFIG."
  (straight-use-package 'diredfl)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "-alhoG")

  (use-package dired-single
    :config
    (evil-define-key 'normal dired-mode-map
      "h" 'dired-single-up-directory
      "l" 'dired-single-buffer))


  (use-package peep-dired
    :config
    (evil-define-key 'normal peep-dired-mode-map
      (kbd "<SPC>") 'peep-dired-scroll-page-down
      (kbd "C-<SPC>") 'peep-dired-scroll-page-up
      (kbd "j") 'peep-dired-next-file
      (kbd "k") 'peep-dired-prev-file)
    (add-hook 'peep-dired-hook 'evil-normalize-keymaps)

    (evil-define-key 'normal dired-mode-map (kbd "p") 'peep-dired))

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
