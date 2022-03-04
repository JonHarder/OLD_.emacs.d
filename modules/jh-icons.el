(use-package all-the-icons)

;;; TODO: doesn't actually start the mode?
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'jh-icons)
;;; jh-icons.el ends here
