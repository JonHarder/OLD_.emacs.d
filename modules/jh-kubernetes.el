;;; kubernetes --- Summary

;;; Commentary:

;;; Code:

(defun modules/kubernetes--load (config)
  (straight-use-package 'kubernetes)
  (straight-use-package 'kubernetes-evil))

(provide 'jh-kubernetes)
;;; jh-kubernetes.el ends here
