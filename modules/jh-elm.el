;;; elm --- Summary

;;; Commentary:

;;; Code:
(defun modules/elm--load (config)
  "Load configuration for elm, using CONFIG."
  (straight-use-package 'elm-mode)
  
  (use-package flycheck-elm
    :after (flycheck)
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-elm-setup)))

(provide 'jh-elm)
;;; jh-elm.el ends here
