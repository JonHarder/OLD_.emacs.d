(defun modules/deft--load (config)
  (use-package deft
    :config
    (setq deft-extensions '("org")
          deft-directory "~/zettelkasten")))
