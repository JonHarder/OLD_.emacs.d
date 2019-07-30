;;; elm --- Summary

;;; Commentary:

;;; Code:
(use-package elm-mode
  :straight t)

(use-package flycheck-elm
  :straight t
  :after (flycheck)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

(provide 'jh-elm)
;;; jh-elm.el ends here
