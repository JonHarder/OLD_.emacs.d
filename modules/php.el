(defun ywb-php-lineup-arglist-intro (langelem)
  "Line up arguments at the start according to LANGELEM."
  (save-excursion
    (goto-char (cdr langelem))
    (vector (+ (current-column) c-basic-offset))))


(defun ywb-php-lineup-arglist-close (langelem)
  "Line up the closing argument list according to LANGELEM."
  (save-excursion
    (goto-char (cdr langelem))
    (vector (current-column))))


(defun my-php-mode-hook ()
  "Configuration for php."
  (setq indent-tabs-mode nil
        c-basic-offset 4
        php-template-compatibility nil
        php-mode-coding-style 'psr2)
  (subword-mode 1)
  (setq ac-sources '(ac-source-php))
  (ac-php-core-eldoc-setup)
  (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
  (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close))


(use-package php-mode
  :mode "\\.php\\'"
  :init
  (add-hook 'php-mode-hook #'my-php-mode-hook))

(setq lsp-intelephense-server-command
      '("intelephense" "--stdio" "--maxMemory=3072"))

(use-package ac-php
  :after php)

(use-package psysh
  :commands psysh)
