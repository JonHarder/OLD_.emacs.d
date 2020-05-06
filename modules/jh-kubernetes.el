;;; kubernetes --- Summary

;;; Commentary:

;;; Code:

(defun modules/kubernetes--load (config)
  (use-package kubernetes
    :demand)
  (use-package kubernetes-evil
    :demand))

(provide 'jh-kubernetes)
;;; jh-kubernetes.el ends here
