(defun modules/dired--load (config)
  "Load configuration for dired, using CONFIG."
  (straight-use-package 'diredfl)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "-alhoG")

  (use-package ranger
    :config
    (setq ranger-show-literal nil))
  (use-package dired-collapse)
  (use-package dired-rainbow)

  (use-package dired-subtree
    :config
    (general-define-key
     :states 'normal
     :keymaps 'dired-mode-map
     "TAB" 'dired-subtree-toggle
     [?\C-\t] 'dired-subtree-cycle)))
