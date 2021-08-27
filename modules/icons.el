(use-package all-the-icons)

;;; TODO: doesn't actually start the mode?
(use-package all-the-icons-completion
  :hook (emacs-startup . all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'icons)
;;; icons.el ends here
