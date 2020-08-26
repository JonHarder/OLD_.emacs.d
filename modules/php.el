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
  (lsp)
  (setq indent-tabs-mode nil
        c-basic-offset 4
        php-template-compatibility nil
        php-mode-coding-style 'psr2)
  (subword-mode 1)
  (setq ac-sources '(ac-source-php))
  (yas-global-mode 1)
  (ac-php-core-eldoc-setup)
  (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
  (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close))


(defun modules/php--load (config)
  "Load configuration for php uising CONFIG."
  (use-package php-mode
    :defer 3
    :init
    (add-hook 'php-mode-hook #'my-php-mode-hook))


  (use-package ac-php)
  (use-package psysh)
  
  (use-package phpunit
    :defer 3))