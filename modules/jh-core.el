;;; core --- Summary
;; Core configuration setup

;;; Commentary:
;; This sets up the basics of the configuration and basic settings
;; (or things that I can't find a better place to put them into)

;;; Code:
(defun modules/core--load (config)
  "Load general core features, configure programming hook using CONFIG."
  

  (setq epa-pinentry-mode 'loopback)

  (straight-use-package 'dash)
  (straight-use-package 'ag)

  (use-package helpful)

  (save-place-mode 1)

  (use-package rg
    :config
    (rg-enable-menu))

  (use-package scratch)
    
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

  (straight-use-package 'fireplace)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)


  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

  (use-package origami
    :config
    (global-origami-mode))

  (use-package flycheck
    :config
    (setq-default flycheck-emacs-lisp-load-path 'inherit)
    (add-hook 'after-init-hook #'global-flycheck-mode))

  (use-package avy)

  
  (straight-use-package 'yaml-mode)
  
  (use-package markdown-mode
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :custom
    (markdown-command "pandoc")))

(provide 'jh-core)
;;; jh-core.el ends here
