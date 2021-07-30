(use-package all-the-icons
  :ensure t)
  
(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
