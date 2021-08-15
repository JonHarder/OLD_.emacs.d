(use-package all-the-icons)

(use-package all-the-icons-completion
  :demand t
  :config
  (all-the-icons-completion-mode 1))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
