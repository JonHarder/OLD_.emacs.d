;; This sets up the basics of the configuration and basic settings
;; (or things that I can't find a better place to put them into)
(defun modules/core--load (config)
  "Load general core features, configure programming hook using CONFIG."
  

  (setq epa-pinentry-mode 'loopback)

  (straight-use-package 'dash)
  (straight-use-package 'ag)

  (use-package helpful)

  (save-place-mode 1)

  (use-package treemacs
    :config
    (treemacs-git-mode 'extended))
  (use-package treemacs-evil)
  (use-package treemacs-projectile)

  (use-package rg
    :config
    (rg-enable-menu))

  (use-package scratch)
    

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