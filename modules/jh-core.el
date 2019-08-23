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
  ;; (display-line-numbers-mode 1)
  (setq whitespace-style '(face tabs space-before-tab empty space-after-tab tab-mark))
  (whitespace-mode 1)
  (highlight-indent-guides-mode 1))


(defun modules/core--load (config)
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
    (setq mac-option-modifier nil
          mac-command-modifier 'meta))
  
  (setq-default indent-tabs-mode nil
                tab-width 4)
  
  (show-paren-mode 1)
  (electric-pair-mode 1)
  
  (require 'whitespace)
  
  
  (use-package highlight-indent-guides
    :straight t
    :init
    (setq highlight-indent-guides-method 'column
          highlight-indent-guides-responsive 'top))

  (add-hook 'prog-mode-hook #'jh/prog-mode-hook)

  (require 'server)
  (when (not (server-running-p))
    (server-start))

  (use-package dockerfile-mode
    :straight t
    :mode "\\.Dockerfile")
  
  
  (use-package dash
    :straight t)

  (use-package flycheck
    :straight t
    :config
    (add-hook 'after-init-hook #'global-flycheck-mode))
  
  
  (use-package yaml-mode
    :straight t)
  
  (use-package rainbow-delimiters
    :straight t
    :config
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

  (use-package markdown-mode
    :straight t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init
    (setq markdown-command "pandoc")))

(provide 'jh-core)
;;; jh-core.el ends here
