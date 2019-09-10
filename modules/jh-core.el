;;; core --- Summary
;; Core configuration setup

;;; Commentary:
;; This sets up the basics of the configuration including use-package,
;; and basic settings (or things that I can't find a better place to put them into)

;;; Code:
(defun alist-keys (alist)
  "Get list of keys in the ALIST."
  (mapcar #'car alist))


(defun jh/prog-mode-hook ()
  "Settings that should be enabled or disabled for all programming modes."
  (setq-default whitespace-style '(face tabs space-before-tab empty space-after-tab tab-mark))
  (whitespace-mode 1))


(defun modules/core--load (config)
  "Load general core features, configure programming hook using CONFIG."
  (defalias 'yes-or-no-p 'y-or-n-p)
  
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  
  ;; core settings behavior
  (setq inhibit-startup-message t
        initial-scratch-message ""
        dired-listing-switches "-alh"
        ring-bell-function 'ignore
        mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
        mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
        mouse-wheel-follow-mouse 't ;; scroll window under mouse
        scroll-step 1 ;; keyboard scroll one line at a time
        backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
        custom-file "~/.emacs.d/custom.el")
  
  (load custom-file 'noerror)
  
  (when (eq system-type 'darwin)
    (setq-default
     mac-option-modifier nil
     mac-command-modifier 'meta
     indent-tabs-mode nil
     tab-width 4))
  
  (show-paren-mode 1)
  (electric-pair-mode 1)
  
  (require 'whitespace)

  (use-package indent-guide
    :hook ((prog-mode . indent-guide-mode)
           (hcl-mode . indent-guide-mode)))

  (add-hook 'prog-mode-hook #'jh/prog-mode-hook)

  (require 'server)
  (when (not (server-running-p))
    (server-start))

  (use-package dockerfile-mode
    :mode "\\.Dockerfile")
  
  
  (straight-use-package 'dash)

  (use-package flycheck
    :config
    (add-hook 'after-init-hook #'global-flycheck-mode))
  
  
  (straight-use-package 'yaml-mode)
  
  (use-package rainbow-delimiters
    :config
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

  (use-package markdown-mode
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :custom
    (markdown-command "pandoc")))

(provide 'jh-core)
;;; jh-core.el ends here
