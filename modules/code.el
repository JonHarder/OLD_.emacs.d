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

  (use-package conf-mode
    :mode "\\.env")

  ;; language server support
  (use-package lsp-mode
    :config
    (setq lsp-idle-delay 0.500
          lsp-enable-file-watchers nil)
    :hook
    ((lsp-mode . lsp-enable-which-key-integration)
     (lsp-mode . lsp-modeline-code-actions-mode)))

  (use-package lsp-ui
    :config
    (setq lsp-ui-doc-position 'at-point))

  (setq-default default-tab-width 4
                c-basic-offset 4
                tab-width 4)

  (defun jh/prog-mode-hook ()
    "Settings that should be enabled or disabled for all programming modes."
    (setq-default whitespace-style '(face space-before-tab line-tail empty space-after-tab))
    (when (alist-get :highlight-line jh/config nil)
      (hl-line-mode t))
    (whitespace-mode 1))


  (add-hook 'c-mode-hook (lambda () (electric-pair-mode 1)))

  (mapc (lambda (hook) (add-hook hook #'lsp-deferred))
        '(c-mode-hook
          go-mode-hook
          php-mode-hook
          python-mode-hook
          dockerfile-mode-hook
          json-mode-hook))

  (add-hook 'prog-mode-hook #'jh/prog-mode-hook config))
