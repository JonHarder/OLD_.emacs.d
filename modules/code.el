;;; code --- Configuration for coding systems. -*- lexical-binding: t -*-

;;; Commentary:
;;; Configuration for programming environments apart from any
;;; language specific configuration, which can be found in
;;; dedicated modules, e.g. "modules/php.el" for php configuration.

;;; Code:
;;;; Requirements
;; (require 'flycheck)
(require 'use-package)


;;;; mode hook functions
(defun jh/yaml-mode-hook ()
  "Configuration to be enabled an yaml buffers."
  (hl-line-mode +1))

(defun jh/prog-mode-hook ()
  "Settings that should be enabled or disabled for all programming modes."
  (setq-default whitespace-style '(face space-before-tab line-tail empty space-after-tab))
  (if jh/highlight-line
      (hl-line-mode t)
    (hl-line-mode 0))
  (whitespace-mode 1))

(use-package dumb-jump
  :commands dumb-jump-go)

(add-hook 'sh-mode-hook #'flycheck-mode)

(use-package elixir-mode
  :mode "\\.exs")

(use-package conf-mode
  :mode "\\.env")

 ;; language server support
(use-package lsp-mode
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

(use-package hl-todo
  :hook (prog-mode-hook . hl-todo-mode))

(use-package display-line-numbers
  :straight nil
  :hook (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'visual))

(use-package yaml-mode
  :mode "\\ya?ml\'"
  :config
  (add-hook 'yaml-mode-hook #'jh/yaml-mode-hook))

(add-hook 'php-mode-hook (lambda () (electric-pair-mode 1)))
(add-hook 'prog-mode-hook #'jh/prog-mode-hook)

(add-hook 'c-mode-hook 'electric-pair-mode)
(general-define-key
 :keymaps 'c-mode-map
 :states 'insert
 "DEL" 'c-hungry-delete-backwards)

(provide 'code)
;;; code.el ends here
