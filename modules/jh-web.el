;;; web --- Summary
;; General set of configuration aimed at making
;; developing for the web.

;;; Commentary:

;;; Code:

(defun modules/web--load (config)
  (use-package emmet-mode
    :straight t
    :hook 'sgml-mode)
  
  (use-package rainbow-mode
    :straight t
    :hook (vue-mode css-mode))
  
  (straight-use-package 'vue-mode)

  (straight-use-package 'rjsx-mode)
  
  (use-package typescript-mode
    :straight t)

  (use-package web-mode
    :straight t
    :mode "\\.html\\'"))

(provide 'jh-web)
;;; jh-web.el ends here
