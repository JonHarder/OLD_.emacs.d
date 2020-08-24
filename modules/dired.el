(defun modules/dired--load (config)
  "Load configuration for dired, using CONFIG."
  (straight-use-package 'ranger)
  (straight-use-package 'diredfl)

  (use-package dired-narrow
    :config
    (general-define-key
     :states 'normal
     :keymaps 'dired-mode-map
     "/" 'dired-narrow))

  (use-package dired-collapse)
  (use-package dired-rainbow)

  (use-package dired-subtree
    :config
    (general-define-key
     :states 'normal
     :keymaps 'dired-mode-map
     "TAB" 'dired-subtree-toggle
     [?\C-\t] 'dired-subtree-cycle)))
