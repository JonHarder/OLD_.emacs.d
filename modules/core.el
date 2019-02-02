;;; core --- Summary
;; Core configuration setup

;;; Commentary:
;; This sets up the basics of the configuration including use-package,
;; and basic settings (or things that I can't find a better place to put them into)

;;; Code:
(defcustom jh/color-theme "material-light"
  "The color theme to use."
  :group 'jh)

(defgroup jh nil
  "Group for storing generic customization for me."
  :group 'convenience)

(defcustom jh/font "Fira Code"
  "The font to use for all text."
  :group 'jh)
(defcustom jh/font-size 17
  "The size of font to use."
  :group 'jh)

(defalias 'yes-or-no-p 'y-or-n-p)


;; core settings behavior
(setq inhibit-startup-message t)


(show-paren-mode 1)

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))


(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      scroll-step 1) ;; keyboard scroll one line at a time

;; store all the backup files (the ones that end with ~) in a dedicated folder.
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))


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
  :disabled t
  :ensure t
  :config
  (dashboard-setup-startup-hook))


(use-package which-key
  :ensure t
  :config
  (which-key-mode))


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
  :config
  (projectile-mode +1))


(defun find-init-file ()
  "Open your init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))


(defun jh/term ()
  (interactive)
  (ansi-term "/usr/local/bin/fish"))


(defun init-bindings ()
  "Define the keyboard bindings."
  (use-package general
    :ensure t
    :config
    (general-create-definer space-leader :prefix "SPC")
    (space-leader
      :keymaps 'normal
      ";" 'eval-expression
      ;; find manipulation
      "f f" 'find-file
      "f s" 'save-buffer
      "f i" 'find-init-file
      ;; buffer manipulation
      "b d" 'evil-delete-buffer
      ;; help
      "h f" 'describe-function
      "h k" 'describe-key
      "h v" 'describe-variable
      ;; window manipulation
      "w /" 'evil-window-vsplit
      "w -" 'evil-window-split
      "w w" 'evil-window-next
      "w m" 'delete-other-windows
      "w c" 'delete-window
      ;; git stuff (using magit)
      "g s" 'magit-status
      ;; project stuff
      "p f" 'project-find-file
      ;; jumpin an stuff
      "j d" 'xref-find-definitions
      ;; evalin stuff
      "e e" 'eval-expression
      ;; searchin stuff
      "s g" 'how-do-i-google
      "s s" 'how-do-i
      "s d" 'how-do-i-ddg
      "s o" 'how-do-i-so
      ;; org
      "o c" 'org-ctrl-c-ctrl-c
      "o a" 'org-archive-subtree
      "o t" 'org-todo
      ;; font stuff
      "=" 'jh/increse-font-size
      "-" 'jh/decrease-font-size
      ;; "applications"
      "a a" 'org-agenda
      "a t" 'jh/term))

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

  (use-package ivy
    :after (counsel general evil)
    :ensure t
    :init
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers 1
	  enable-recursive-minibuffers t)
    :config
    (space-leader
      :keymaps 'normal
      "SPC" 'counsel-M-x
      "b b" 'ivy-switch-buffer
      "/" 'swiper)))


(use-package yaml-mode
  :ensure t)

(use-package ansible
  :ensure t)


;;; python configuration
(use-package flycheck-pycheckers
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))



(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


(init-bindings)


(provide 'core)
;;; core.el ends here
