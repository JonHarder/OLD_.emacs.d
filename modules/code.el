;;; code --- Configuration for coding systems.

;;; Commentary:
;;; Configuration for programming environments apart from any
;;; language specific configuration, which can be found in
;;; dedicated modules, e.g. "modules/php.el" for php configuration.

;;; Code:
;;;; Requirements
(require 'flycheck)
(require 'use-package)
(require 'straight)


;;;; mode hook functions
(defun jh/yaml-mode-hook ()
  "Configuration to be enabled an yaml buffers."
  (hl-line-mode +1))

(defun jh/prog-mode-hook ()
  "Settings that should be enabled or disabled for all programming modes."
  (setq-default whitespace-style '(face space-before-tab line-tail empty space-after-tab))
  (when (alist-get :highlight-line jh/config nil)
    (hl-line-mode t))
  (whitespace-mode 1))

(defun modules/code--load (config)
  "Module definition for generic programming, configured by CONFIG."
  (straight-use-package 'dumb-jump)

  (use-package editorconfig
    :defer 6
    :config
    (editorconfig-mode 1))

  (use-package linum-relative
    :config
    (setq linum-relative-backend 'display-line-numbers-mode))

  (add-hook 'sh-mode-hook #'flycheck-mode)

  (use-package plantuml-mode
    :custom
    (plantuml-default-exec-mode 'jar)
    (org-plantuml-jar-path "~/plantuml.jar")
    :config
    (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
    (unless (file-exists-p "~/plantuml.jar")
      (plantuml-download-jar)))

  (use-package conf-mode
    :mode "\\.env")

  ;; language server support
  (use-package lsp-mode
    :config
    (setq lsp-idle-delay 0.500
          lsp-enable-file-watchers nil)

    (mapc (lambda (hook) (add-hook hook #'lsp-deferred))
          '(c-mode-hook
            go-mode-hook
            php-mode-hook
            typescript-mode
            dockerfile-mode-hook
            json-mode-hook))
    :hook
    ((lsp-mode . lsp-enable-which-key-integration)
     (lsp-mode . lsp-modeline-code-actions-mode)))

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
    :config
    (add-hook 'yaml-mode-hook #'jh/yaml-mode-hook))

  (add-hook 'c-mode-hook (lambda () (electric-pair-mode 1)))
  (add-hook 'php-mode-hook (lambda () (electric-pair-mode 1)))
  (add-hook 'prog-mode-hook #'jh/prog-mode-hook config))

(provide 'code)
;;; code.el ends here
