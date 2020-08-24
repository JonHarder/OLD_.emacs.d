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

  ;; language server support
  (use-package lsp-mode
    :config
    (setq lsp-idle-delay 0.500
          lsp-enable-file-watchers nil)
    :hook
    ((lsp-mode . lsp-enable-which-key-integration)
     (lsp-mode . lsp-modeline-code-actions-mode)))

  (use-package lsp-ui
    :requires lsp-mode flycheck
    :config
    (setq lsp-ui-sideline-show-diagnostics t
          lsp-ui-sideline-show-code-actions t
          lsp-ui-doc-enable nil
          ;; lsp-ui-doc-position 'top
          lsp-ui-peek-enable t
          lsp-ui-peek-show-directory t
          lsp-ui-doc-include-signature t)
    :hook
    ((lsp-mode . lsp-ui-mode)))

  (use-package company-lsp
    :config
    (push 'company-lsp company-backends)
    (setq company-transformers nil
          company-lsp-async t
          company-lsp-cache-candidates nil))

  (defun jh/prog-mode-hook ()
    "Settings that should be enabled or disabled for all programming modes."
    (setq-default whitespace-style '(face tabs space-before-tab line-tail empty space-after-tab tab-mark))
    (hl-line-mode)
    (whitespace-mode 1)
    (linum-relative-mode 1))

  (add-hook 'prog-mode-hook #'jh/prog-mode-hook))
