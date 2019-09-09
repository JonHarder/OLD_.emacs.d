;;; web --- Summary
;; General set of configuration aimed at making
;; developing for the web.

;;; Commentary:

;;; Code:

(defun modules/web--load (config)
  "Load configuration for web based modes using CONFIG."
  (use-package emmet-mode
    :hook 'sgml-mode)
  
  (use-package rainbow-mode
    :hook (vue-mode css-mode))
  
  (use-package vue-mode)
  
  (use-package typescript-mode)

  (use-package web-mode
    :mode "\\.html\\'"))

(provide 'jh-web)
;;; jh-web.el ends here
