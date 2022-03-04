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

(use-package lsp-ui
  :after lsp
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-background 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05)
  (setq lsp-ui-peek-enable t))

(setq-default default-tab-width 4
              c-basic-offset 4
              tab-width 4)

(use-package hl-todo
  :hook (emacs-startup . global-hl-todo-mode))

(add-to-list 'auto-mode-alist '("Pipfile" . conf-toml-mode))

(use-package display-line-numbers
  :straight nil
  :hook (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'visual)
  :general
  (:prefix "SPC"
   :states 'normal
   :keymaps 'prog-mode-map
   "c n" 'display-line-numbers-mode))

(use-package yaml-mode
  :mode "\\ya?ml\'"
  :config
  (add-hook 'yaml-mode-hook #'jh/yaml-mode-hook))

(add-hook 'php-mode-hook (lambda () (electric-pair-mode 1)))
(add-hook 'prog-mode-hook #'jh/prog-mode-hook)

(use-package applescript-mode
  :mode "\\.applescript\'")

(add-hook 'c-mode-hook 'electric-pair-mode)
(general-define-key
 :keymaps 'c-mode-map
 :states 'insert
 "DEL" 'c-hungry-delete-backwards)

(provide 'jh-code)
;;; jh-code.el ends here
