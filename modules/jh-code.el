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

(use-package highlight-indent-guides
  :custom
  ;; (highlight-indent-guides-method 'line)
  (highlight-indent-guides-method 'column)
  :config
  (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
  (add-hook 'yaml-mode-hook #'highlight-indent-guides-mode))

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

(use-package ansible
  :config
  (dir-locals-set-class-variables 'ansible-directory
                                  '((yaml-mode . ((eval . (ansible 1))))))
  (dir-locals-set-directory-class
   "~/Kipsu/ansible-playbooks/playbooks" 'ansible-directory)
  (dir-locals-set-directory-class
   "~/Kipsu/ansible-playbooks/roles" 'ansible-directory))

 ;; language server support
(use-package lsp-mode
  :config
  (setq lsp-idle-delay 0.500
        lsp-enable-file-watchers nil)
  :hook ((c-mode go-mode php-mode dockerfile-mode) . lsp-deferred))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-moude nil)
  (lsp-ui-doc-border (face-background 'default))
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-delay 0.05)
  (lsp-ui-peek-enable t))

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
