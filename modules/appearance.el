;;; appearance -- Configured the visual appearance of emacs

;;; Commentary:

;;; Code:
(require 'seq)
(require 'use-package)
(require 'contrib "~/.emacs.d/contrib.el")


(defvar jh/themes
  '(("humanoid" .
     (:package humanoid-themes
      :light humanoid-light
      :dark humanoid-dark))
    ("modus" .
     (:package modus-themes
      :light modus-operandi
      :dark modus-vivendi))
    ("spacemacs" .
     (:package spacemacs-theme
      :light spacemacs-light
      :dark spacemacs-dark))
    ("solarized" .
     (:package doom-themes
      :light doom-solarized-light
      :dark doom-solarized-dark))
    ("gruvbox" .
     (:package doom-themes
      :light doom-gruvbox-light
      :dark doom-gruvbox))
    ("doom" .
     (:package doom-themes
      :light doom-one-light
      :dark doom-one))
    ("dracula" .
     (:package doom-themes
      :dark doom-dracula
      :light doom-dracula))))

(defvar jh/current-theme
  (alist-get :theme jh/config))


(defun jh/theme-customizations (theme)
  "Perform any theme specific configuration for a given THEME."
  (cond
   ((string-prefix-p "modus-" (symbol-name theme))
    (use-package modus-themes
      :custom
      (modus-themes-slanted-constructs t)
      (modus-themes-mode-line 'accented)
      (modus-themes-completion 'opinionated)
      (modus-themes-org-blocks 'rainbow)
      (modus-themes-bold-constructs t)
      (modus-themes-headings '((t . rainbow-section)))
      (modus-themes-syntax 'alt-syntax)
      (modus-themes-completions 'opinionated)
      (modus-themes-scale-headings t)
      (modus-themes-paren-match 'intense)))

   ;;; doom themes
   ((string-prefix-p "doom-" (symbol-name theme))
    (use-package doom-themes
      :config
      (doom-themes-org-config)
      (doom-themes-enable-org-fontification)
      :custom
      (doom-themes-enable-bold t)
      (doom-themes-enable-italic t)
      (doom-themes-padded-modeline nil)
      (doom-one-light-brighter-comments t)
      (doom-one-brighter-comments t)
      (doom-solarized-light-brighter-comments t)
      (doom-solarized-dark-brighter-text t)
      (doom-solarized-dark-brighter-modeline t)
      (doom-solarized-dark-brighter-comments t)))))


(defun jh/mac-is-dark-mode-p ()
  "Determine if MacOS dark theme is enabled."
  (interactive)
  (string-equal "Dark" (string-trim (shell-command-to-string "defaults read -g AppleInterfaceStyle"))))

(defvar jh/dark-mode (jh/mac-is-dark-mode-p) "Boolean which tracks mac system level dark mode.")

(defun jh/set-theme-to-system (light-theme dark-theme &optional package)
  "Set the theme to LIGHT-THEME if MacOS is not in dark mode, set to DARK-THEME otherwise, using PACKAGE to install themes if given.

Utilizes `jh/load-theme' under the hood."
  (setq jh/dark-mode (jh/mac-is-dark-mode-p))
  (if jh/dark-mode
      (jh/load-theme dark-theme package)
    (jh/load-theme light-theme package)))


(defun jh/set-theme (theme)
  "Load THEME, disabling other enabled themes.

Low level theme manipulation.  Use `select-theme' for higher level
function to load a particular theme."
  (let ((other-themes (seq-filter (lambda (other-theme)
                                    (not (string-equal theme other-theme)))
                                  custom-enabled-themes)))
    (mapc 'disable-theme other-themes)
    (jh/theme-customizations theme)
    (load-theme theme t)))


(defun jh/load-theme (theme &optional theme-package)
  "Enable THEME, optionally found in THEME-PACKAGE.

 If theme is not a built in theme, and not present on the machine, it will be installed."
  (unless (memq theme custom-enabled-themes)
    (package-install theme-package)
    (jh/set-theme theme)))

(defun jh/theme-config ()
  "Get the configuration for the chosen theme."
  (alist-get jh/current-theme jh/themes nil nil #'string-equal))

(defun jh/theme-property (prop)
  "Get a keyword PROP from the theme configuration."
  (let ((theme-config (jh/theme-config)))
    (plist-get theme-config prop)))

(defun jh/theme-dark ()
  "Get the dark theme from theme config."
  (jh/theme-property :dark))

(defun jh/theme-light ()
  "Get the light theme from theme config."
  (jh/theme-property :light))

(defun jh/theme-package ()
  "Get the package name from the theme config."
  (jh/theme-property :package))

(defun select-theme (name)
  "Select the given theme, indexed by NAME.

Uses the dark or light variant depending on system setting."
  (interactive (list (completing-read
                      "Theme: "
                      (contrib/alist-keys jh/themes))))
  (setq jh/current-theme name)
  (jh/set-theme-to-system
   (jh/theme-light)
   (jh/theme-dark)
   (jh/theme-package)))

(defun reload-theme ()
  (interactive)
  (select-theme jh/current-theme))
  

(defvar jh/theme-switch-timer nil "Timer used to schedule querying OSX system color preference.")

(defun modules/appearance--load (config)
  "Load appearance settings based off of CONFIG."
  (require 'whitespace)

  (blink-cursor-mode -1)
  (show-paren-mode 1)
  (global-prettify-symbols-mode +1)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)

  (use-package diff-hl
    :config
    (setq diff-hl-draw-borders nil)
    (setq diff-hl-side 'left)
    :hook ((after-init . global-diff-hl-mode)))

  (fringe-mode '(8 . 8))
  (setq-default fringes-outside-margins nil)
  (setq-default indicate-buffer-boundaries nil)
  (setq-default indicate-empty-lines nil)
  (setq-default overflow-newlines-into-fringe t)

  (use-package rainbow-delimiters
    :functions 'rainbow-delimiters-mode-enable
    :config
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode-enable))

  (let* ((font-name (alist-get :font config))
         (font-size (alist-get :font-size config))
         (font (format "%s %s" font-name font-size))
         (theme-name (alist-get :theme config)))

    (select-theme theme-name)

    (when jh/theme-switch-timer
      (cancel-timer jh/theme-switch-timer))
    (setq jh/theme-switch-timer
          (run-with-idle-timer 2 1 #'reload-theme))

    (set-frame-font font)
    (add-to-list 'default-frame-alist `(font . ,font))))
(provide 'appearance)
;;; appearance.el ends here
