;;; terraform --- Summary


;;; Commentary:


;;; Code:

(defun modules/terraform--load (config)
  "Install terraform mode and ignore CONFIG."
  (straight-use-package 'terraform-mode)
  (straight-use-package 'company-terraform)

  (defun jh/terraform-mode-hook ()
    (company-terraform-init))

  (add-hook 'terraform-mode-hook #'jh/terraform-mode-hook))


(provide 'jh-terraform)
;;; jh-terraform.el ends here
