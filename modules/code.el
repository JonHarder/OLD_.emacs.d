;;; code --- Configuration for coding systems. -*- lexical-binding: t -*-

;;; Commentary:
;;; Configuration for programming environments apart from any
;;; language specific configuration, which can be found in
;;; dedicated modules, e.g. "modules/php.el" for php configuration.

;;; Code:
;;;; Requirements
(require 'flycheck)
(require 'use-package)


;;;; mode hook functions
(defun jh/yaml-mode-hook ()
  "Configuration to be enabled an yaml buffers."
  (hl-line-mode +1))

(defun jh/prog-mode-hook ()
  "Settings that should be enabled or disabled for all programming modes."
  (setq-default whitespace-style '(face space-before-tab line-tail empty space-after-tab))
  (if (alist-get :highlight-line jh/config nil)
      (hl-line-mode t)
    (hl-line-mode 0))
  (whitespace-mode 1))

(defun modules/code--load (config)
  "Module definition for generic programming, configured by CONFIG."
  (use-package dumb-jump
    :ensure t
    :commands dumb-jump-go)

  (add-hook 'sh-mode-hook #'flycheck-mode)

  (use-package elixir-mode
    :ensure t
    :mode "\\.exs")

  (use-package conf-mode
    :ensure t
    :mode "\\.env")

  ;; language server support
  (use-package lsp-mode
    :ensure t
    :config
    (setq lsp-idle-delay 0.500
          lsp-enable-file-watchers nil)

    :hook ((c-mode go-mode php-mode dockerfile-mode) . lsp-deferred))

  ;;; This has been reported as the source of massive slowdowns
  ;;; in lsp mode enabled files, disabling for now until more
  ;;; thourough tests can be done.
  ;; (use-package lsp-ui
  ;;   :config
  ;;   (setq lsp-ui-doc-enable nil))

  (setq-default default-tab-width 4
                c-basic-offset 4
                tab-width 4)

  (use-package yaml-mode
    :ensure t
    :mode "\\ya?ml\'"
    :config
    (add-hook 'yaml-mode-hook #'jh/yaml-mode-hook))

  (add-hook 'c-mode-hook (lambda () (electric-pair-mode 1)))
  (add-hook 'php-mode-hook (lambda () (electric-pair-mode 1)))
  (add-hook 'prog-mode-hook #'jh/prog-mode-hook config))

(provide 'code)
;;; code.el ends here
