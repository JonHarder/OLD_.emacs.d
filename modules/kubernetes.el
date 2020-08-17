(defun modules/kubernetes--load (config)
  (use-package kubernetes
    :defer 2)
  (use-package kubernetes-evil
    :defer 2))
