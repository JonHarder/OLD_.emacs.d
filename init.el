(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
You version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(package-refresh-contents)


(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
;;; DONT TOUCH STUFF ABOVE HERE (probably...hopefuly)


(defun load-module (module-name &optional module-path)
  "Load the module called MODULE-NAME defined in my modules folder, or in MODULE-PATH if given."
  (let* ((module-path (or module-path
                          (concat user-emacs-directory "modules/")))
         (path (concat module-path (symbol-name module-name) ".el")))
    (require module-name path)))

(defgroup jh nil
  "Group for storing generic customization for me."
  :group 'convenience)

(defcustom jh/font "Fira Code"
  "The font to use for all text."
  :group 'jh)
(defcustom jh/font-size 15
  "The size of font to use."
  :group 'jh)

(defcustom jh/color-theme "solarized-light"
  "The color theme to use."
  :group 'jh)

(defalias 'yes-or-no-p 'y-or-n-p)


(load-module 'org)
(load-module 'appearance)
(load-module 'evil)
(load-module 'search)
(load-module 'php)


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


(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))


;; general package
(use-package dashboard
  :after evil-org
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
  (setq markdown-command "multimarkdown"))


(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))


(defun find-init-file ()
  "Open your init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))


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
      "a" 'org-agenda))

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

; DONT TOUCH STUFF BELOW HERE -----------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(package-selected-packages
   (quote
    (git-gutter spacemacs-theme zenburn-theme evil-collection dracula-theme challenger-deep-theme rainbow-delimiters org-bullets evil-org markdown-mode dashboard flycheck-pycheckers amx which-key projectile evil-magit ansible yaml-mode solarized-theme counsel ivy magit general php-mode use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
