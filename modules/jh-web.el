;;; web --- Summary
;; General set of configuration aimed at making
;; developing for the web.

;;; Commentary:

;;; Code:

(use-package emmet-mode
  :ensure t
  :hook 'sgml-mode)


(use-package rainbow-mode
  :ensure t
  :hook (vue-mode css-mode))


(use-package vue-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode "\\.html\\'")
  


(provide 'jh-web)
;;; jh-web.el ends here
