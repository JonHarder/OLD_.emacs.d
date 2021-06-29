(defun modules/personal--load (config)
  "Personal configuration using CONFIG."
  (use-package ledger-mode)
  (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)

  (defun personal-todo ()
    (interactive)
    (save-excursion
      (find-file "~/Org/personal.org")
      (org-agenda-set-restriction-lock 'file)
      (org-agenda-list))))
