;;; php --- Summary
;; 

;;; Commentary:

;;; Code:
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


(defun modules/php--load (config)
  (use-package ac-php
    :straight t)

  (use-package php-mode
    :straight t
    :init
    (add-hook 'php-mode-hook #'my-php-mode-hook))
  
  
  (use-package phpunit
    :straight t)
  
  (use-package lsp-mode
    :straight t
    :hook (php-mode . lsp)
    :commands lsp
    :config
    (setq lsp-prefer-flymake nil))
  
  (use-package lsp-mode
    :straight t
    :config
    (setq lsp-prefer-flymake nil)
    :hook (php-mode . lsp)
    :commands lsp)
  
  (use-package lsp-ui
    :straight t
    :requires lsp-mode flycheck
    :config
    (setq lsp-ui-doc-enable t
          lsp-ui-doc-use-childframe t
          lsp-ui-doc-position 'top
          lsp-ui-doc-include-signature t
          lsp-ui-sideline-enable nil
          lsp-ui-flycheck-enable t
          lsp-ui-flycheck-list-position 'right
          lsp-ui-flycheck-live-reporting t
          lsp-ui-peek-enable t
          lsp-ui-peek-list-width 60
          lsp-ui-peek-peek-height 25
          lsp-ui-sideline-enable nil)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)))

(provide 'jh-php)
;;; jh-php.el ends here
