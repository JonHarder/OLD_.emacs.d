;;; personal --- Summary
;; random packages not related to coding or work

;;; Commentary:
;; load modules

;;; Code:
(defun modules/personal--load (config)
  "Personal configuration using CONFIG."
  (straight-use-package 'ledger-mode)
  (add-to-list 'evil-emacs-state-modes 'ledger-report-mode))
                         

(provide 'jh-personal)
;;; jh-personal.el ends here
