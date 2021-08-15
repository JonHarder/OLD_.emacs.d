;;; -*- lexical-binding: t -*-

;;; Code:
(use-package kubernetes
  :commands kubernetes-overview)
(use-package kubernetes-evil
  :after kubernetes)

(provide 'kubernetes)
;;; kubernetes.el ends here
