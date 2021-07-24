(defun modules/icons--load (config)
  "Load icon related configuration, using CONFIG as necessary."
  (use-package all-the-icons
    :ensure t)
  
  (use-package all-the-icons-dired
    :ensure t
    :after all-the-icons
    :init
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))
