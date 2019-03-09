;;; icons --- Summary
;; anything to do with fancying up the display, mostly icons.

;;; Commentary:

;;; Code:
(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))


(provide 'icons)
;;; icons.el ends here
