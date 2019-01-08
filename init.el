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





(setq org-agenda-files
      '("~/Org"))


(unless (package-installed-p 'use-package)
  (package-refreshh-contents)
  (package-install 'use-package))
(require 'use-package)
;;; DONT TOUCH STUFF ABOVE HERE (probably...hopefuly)


;; store all the backup files (the ones that end with ~) in a dedicated folder.
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))


(defun alist-keys (alist)
  "Get list of keys in the ALIST."
  (mapcar #'car alist))


;;; Utility functions

;; SEARCHIN STUFF=============================
(defvar search-engines
  '((google . "https://google.com/search?q=")
    (ddg . "https://ddg.gg/?q=")
    (stack-overflow . "https://stackoverflow.com/search?q=")))

(defun how-do-i (engine search-term)
  "Use a specified search ENGINE to query your SEARCH-TERM."
  ;; use region if active for search-term
  (interactive (list
		(completing-read "Enigne: " (alist-keys search-engines))
		(read-string "Search: ")))
  (let ((url (cdr (assoc (intern engine) search-engines))))
    (browse-url (concat url search-term))))

(defun how-do-i-google (search-term)
  "Google search for SEARCH-TERM."
  (interactive "sSearch: ")
  (how-do-i "google" search-term))

(defun how-do-i-ddg (search-term)
  "DuckDuckGo search for SEARCH-TERM."
  (interactive "sSearch: ")
  (how-do-i "ddg" search-term))

(defun how-do-i-so (search-term)
  "Stack Overflow search for SEARCH-TERM."
  (interactive "sSearch: ")
  (how-do-i "stack-overflow" search-term))
;; ======================================


(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))


(use-package which-key
  :ensure t
  :config
  (which-key-mode))


;; UI, THEMEIMG, etc.
(defun init-ui ()
  "Initialize the visual components of Emacs."
  (set-frame-font "Fira Code 14" nil t)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (defalias 'yes-or-no-p 'y-or-n-p)

  (use-package solarized-theme
    :ensure t
    :config
    (load-theme 'solarized-light t)))


;;; PACKAGE CONFIGURATION

(use-package dash
  :ensure t)

(use-package magit
  :ensure t)


(defun load-module (module-name &optional module-path)
  "Load the module called MODULE-NAME defined in my modules folder, or in MODULE-PATH if given."
  (let ((path (or module-path (concat user-emacs-directory "modules/"))))
    (require module-name (concat path (symbol-name module-name) ".el"))))


(load-module 'evil)


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
      ;; find manipulation
      "f f" 'find-file
      "f s" 'save-buffer
      "f i" 'find-init-file
      ;; buffer manipulation
      "b d" 'evil-delete-buffer
      ;; help
      "h f" 'describe-function
      "h k" 'describe-key
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

(use-package ac-php
  :ensure t)

(defun my-php-mode-hook ()
  "Configuration for php."
  (setq indent-tabs-mode t
	c-basic-offset 4
	php-template-compatibility nil)
  (subword-mode 1)

  (auto-complete-mode t)
  (setq ac-sources '(ac-source-php))
  (yas-global-mode 1)
  (ac-php-core-eldoc-setup))

  
(use-package php-mode
  :ensure t
  :init
  (add-hook 'php-mode-hook #'my-php-mode-hook))




;;; python configuration
(use-package flycheck-pycheckers
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))


(init-ui)
(init-bindings)



; DONT TOUCH STUFF BELOW HERE -----------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(package-selected-packages
   (quote
    (dashboard flycheck-pycheckers amx which-key projectile evil-magit ansible yaml-mode solarized-theme counsel ivy magit general php-mode use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
