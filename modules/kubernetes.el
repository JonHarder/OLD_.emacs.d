;;; -*- lexical-binding: t -*-
(defun modules/kubernetes--load (config)
  (use-package kubernetes
    :ensure t
    :commands kubernetes-overview)
  (use-package kubernetes-evil
    :ensure t
    :after kubernetes))
