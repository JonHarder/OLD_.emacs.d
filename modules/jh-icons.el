;;; icons --- Summary
;; anything to do with fancying up the display, mostly icons.

;;; Commentary:

;;; Code:
(use-package all-the-icons
  :straight t)

(use-package all-the-icons-dired
  :straight t
  :after all-the-icons
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))


(provide 'jh-icons)
;;; jh-icons.el ends here
