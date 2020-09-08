;; This sets up the basics of the configuration and basic settings
;; (or things that I can't find a better place to put them into)
(defun modules/core--load (config)
  "Load general core features, configure programming hook using CONFIG."
  
  (use-package dashboard
    :config
    (setq dashboard-startup-banner 'logo
          dashboard-center-content t
          dashboard-items '((recents . 5)
                            (bookmarks . 5)
                            (projects . 5)
                            (agenda . 5))
          show-week-agenda-p t)
    (dashboard-setup-startup-hook))

  (setq epa-pinentry-mode 'loopback)

  (straight-use-package 'dash)
  (straight-use-package 'ag)

  (use-package helpful)

  (save-place-mode 1)

  (use-package popwin
    :config
    (popwin-mode 1))

  (use-package zoom
    :config
    (zoom-mode 1))

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
