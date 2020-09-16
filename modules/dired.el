(defun modules/dired--load (config)
  "Load configuration for dired, using CONFIG."
  (straight-use-package 'diredfl)

  (put 'dired-find-alternate-file 'disabled nil)

  (use-package ranger)
  (use-package dired-collapse)
  (use-package dired-rainbow)

  (use-package dired-subtree
    :config
    (general-define-key
     :states 'normal
     :keymaps 'dired-mode-map
     "TAB" 'dired-subtree-toggle
     [?\C-\t] 'dired-subtree-cycle)))
