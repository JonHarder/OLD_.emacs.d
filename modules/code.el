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

  ;; language server support
  (use-package lsp-mode
    :config
    (setq lsp-idle-delay 0.500
          lsp-enable-file-watchers nil)
    :hook
    ((lsp-mode . lsp-enable-which-key-integration)
     (lsp-mode . lsp-modeline-code-actions-mode)))

  (use-package lsp-ui)

  (defun jh/prog-mode-hook ()
    "Settings that should be enabled or disabled for all programming modes."
    (setq-default whitespace-style '(face tabs space-before-tab line-tail empty space-after-tab tab-mark))
    (when (alist-get :highlight-line jh/config nil)
        (hl-line-mode t))
    (whitespace-mode 1))


  (add-hook 'prog-mode-hook #'jh/prog-mode-hook config)
  (add-hook 'prog-mode-hook #'lsp-deferred))
