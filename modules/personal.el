(defun modules/personal--load (config)
  "Personal configuration using CONFIG."
  (straight-use-package 'ledger-mode)
  (add-to-list 'evil-emacs-state-modes 'ledger-report-mode))
