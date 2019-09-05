;;; terraform --- Summary


;;; Commentary:


;;; Code:

(defun modules/terraform--load (config)
  "Install terraform mode and ignore CONFIG."
  (use-package terraform-mode
    :straight t))


(provide 'jh-terraform)
;;; jh-terraform.el ends here
