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



(defcustom jh/color-theme "gruvbox-dark-medium"
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


(load-module 'org)
(load-module 'appearance)
(load-module 'evil)
(load-module 'search)
(load-module 'php)


(show-paren-mode 1)


;; Automatically kill terminal buffers after their process exists
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


(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))


(use-package evil-lispy
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode))
  

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

; DONT TOUCH STUFF BELOW HERE -----------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(cider-lein-command "/Users/jharder/local/bin/lein")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("604ac011fc9bd042bc041330b3a5e5a86e764a46f7e9fe13e2a1f9f83bf44327" "e2fd81495089dc09d14a88f29dfdff7645f213e2c03650ac2dd275de52a513de" "2a9039b093df61e4517302f40ebaf2d3e95215cb2f9684c8c1a446659ee226b9" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "11e57648ab04915568e558b77541d0e94e69d09c9c54c06075938b6abc0189d8" "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" "36c86cb6c648b9a15d849026c90bd6a4ae76e4d482f7bcd138dedd4707ff26a5" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX" . "#dc752f")
     ("XXXX" . "#dc752f")
     ("???" . "#dc752f"))))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (gruvbox-theme fish-mode molokai-theme monokai-theme evil-lispy cider groovy-mode gradle-mode git-gutter zenburn-theme evil-collection dracula-theme challenger-deep-theme rainbow-delimiters org-bullets evil-org markdown-mode dashboard flycheck-pycheckers amx which-key projectile evil-magit ansible yaml-mode solarized-theme counsel ivy magit general php-mode use-package)))
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#1d2021")))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
