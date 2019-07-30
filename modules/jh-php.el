;;; php --- Summary
;; 

;;; Commentary:

;;; Code:

(use-package ac-php
  :straight t)


(defun ywb-php-lineup-arglist-intro (langelem)
  (save-excursion
    (goto-char (cdr langelem))
    (vector (+ (current-column) c-basic-offset))))


(defun ywb-php-lineup-arglist-close (langelem)
  (save-excursion
    (goto-char (cdr langelem))
    (vector (current-column))))


(defun my-php-mode-hook ()
  "Configuration for php."
  (setq indent-tabs-mode nil
        c-basic-offset 4
        php-template-compatibility nil)
  (subword-mode 1)
  (auto-complete-mode t)
  (setq ac-sources '(ac-source-php))
  (yas-global-mode 1)
  (ac-php-core-eldoc-setup)
  (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
  (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close))

  
(use-package php-mode
  :straight t
  :init
  (add-hook 'php-mode-hook #'my-php-mode-hook))


(use-package phpunit
  :straight t)


(provide 'jh-php)
;;; jh-php.el ends here
