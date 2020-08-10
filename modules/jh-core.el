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
    (setq lsp-idle-delay 0.500)
    (setq lsp-enable-file-watchers nil))
  (use-package lsp-ui
    :requires lsp-mode flycheck)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)

  (use-package company-lsp)

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
