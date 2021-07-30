;;; -*- lexical-binding: t -*-

;;; Code:
(use-package kubernetes
  :ensure t
  :commands kubernetes-overview)
(use-package kubernetes-evil
  :ensure t
  :after kubernetes)

(provide 'kubernetes)
;;; kubernetes.el ends here
