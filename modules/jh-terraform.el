;;; terraform --- Summary


;;; Commentary:


;;; Code:

(defun modules/terraform--load (config)
  "Install terraform mode and ignore CONFIG."
  (straight-use-package 'terraform-mode))


(provide 'jh-terraform)
;;; jh-terraform.el ends here
