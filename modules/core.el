;;; core --- Summary
;; Core configuration setup

;;; Commentary:
;; This sets up the basics of the configuration including use-package,
;; and basic settings (or things that I can't find a better place to put them into)

;;; Code:
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
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))


(when (eq system-type 'darwin)
  (setq mac-option-modifier nil
        mac-command-modifier 'meta))


(show-paren-mode 1)
(electric-pair-mode 1)

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))


(defun alist-keys (alist)
  "Get list of keys in the ALIST."
  (mapcar #'car alist))


(use-package dockerfile-mode
  :ensure t
  :mode "\\.Dockerfile")


(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))


(use-package fish-mode
  :ensure t)


(use-package dashboard
  :ensure t
  :init
  (setq dashboard-startup-banner 'logo)
  :config
  (dashboard-setup-startup-hook))


(use-package dash
  :ensure t)


(use-package magit
  :ensure t)


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc"))


(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode +1))


(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; COMPLETION/NARROWING
(use-package counsel
  :ensure t)

(use-package amx
  :ensure t
  :config
  (amx-mode))


(defun swiper--nohighlight (orig-func &rest args)
  "Get rid of the highlighting after exiting swiper."
  (apply orig-func args)
  (evil-ex-nohighlight))

(advice-add 'swiper :around #'swiper--nohighlight)


(use-package yaml-mode
  :ensure t)

(use-package ansible
  :ensure t)


(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


(provide 'core)
;;; core.el ends here
