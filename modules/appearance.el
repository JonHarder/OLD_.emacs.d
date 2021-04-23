;;; appearance -- Configured the visual appearance of emacs

;;; Commentary:

;;; Code:
(require 'seq)
(require 'use-package)
(require 'straight)

(defun jh/pull-theme (theme &optional theme-package)
  "Download THEME-PACKAGE if THEME is not a built in theme."
  (let ((theme-package (if theme-package theme-package (intern (format "%s-theme" theme)))))
    (when (not (memq (intern theme) (custom-available-themes)))
      (straight-use-package theme-package))))


(defun jh/theme-config (theme)
  "Perform any theme specific configuration for a given THEME."
  (cond
   ((string-prefix-p "modus-" theme)
    (use-package modus-themes
      :config
      (modus-themes-load-themes)
      :custom
      (modus-themes-slanted-constructs t)
      (modus-themes-mode-line '3d)
      (modus-themes-completion 'opinionated)
      (modus-themes-org-blocks 'rainbow)
      (modus-themes-bold-constructs t)
      (modus-themes-headings '((t . rainbow-section)))
      (modus-themes-syntax 'yellow-comments-green-strings)
      (modus-themes-completions 'opinionated)
      (modus-themes-scale-headings t)
      (modus-themes-paren-match 'intense)))

   ;;; doom themes
   ((string-prefix-p "doom-" theme)
    (use-package doom-themes
      :config
      (doom-themes-org-config)
      (doom-themes-enable-org-fontification)
      :custom
      (doom-themes-enable-bold t)
      (doom-themes-enable-italic t)
      (doom-themes-padded-modeline nil)
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
  "Set the theme to LIGHT-THEME if MacOS is not in dark mode, set to DARK-THEME otherwise, using PACKAGE to install themes if given."
  (interactive)
  (setq jh/dark-mode (jh/mac-is-dark-mode-p))
  (if jh/dark-mode
      (jh/load-theme dark-theme package)
    (jh/load-theme light-theme package)))


(defun jh/set-theme (theme)
  "Load THEME, disabling other enabled themes."
  (let ((other-themes (seq-filter (lambda (other-theme)
                                    (not (string-equal theme other-theme)))
                                  custom-enabled-themes)))
    (mapc 'disable-theme other-themes)
    (jh/theme-config theme)
    (load-theme (intern theme) t)))


(defun jh/load-theme (&optional theme theme-package)
  "Enable THEME, optionally found in THEME-PACKAGE.

 If theme is not a built in theme, and not present on the machine, it will be installed."
  (interactive)
  (let ((theme (or theme (completing-read "Theme: " (custom-available-themes)))))
    (unless (memq (intern theme) custom-enabled-themes)
      (jh/pull-theme theme theme-package)
      (jh/set-theme theme))))

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
    :config
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode-enable))

  (let* ((color-theme (alist-get :color-theme config))
         (font-name (alist-get :font config))
         (font-size (alist-get :font-size config))
         (font (format "%s %s" font-name font-size))
         (theme-package (alist-get :theme-package config))
         (light-theme (symbol-name (alist-get :light-theme config)))
         (dark-theme (symbol-name (alist-get :dark-theme config))))

    (jh/set-theme-to-system light-theme dark-theme theme-package)

    (when (not (null jh/theme-switch-timer))
      (cancel-timer jh/theme-switch-timer))
    (setq jh/theme-switch-timer
          (run-with-idle-timer 2 1 (lambda (light-theme dark-theme theme-package)
                                     (jh/set-theme-to-system light-theme dark-theme theme-package))
                               light-theme dark-theme theme-package))

    (set-frame-font font)
    (add-to-list 'default-frame-alist `(font . ,font))))
(provide 'appearance)
;;; appearance.el ends here
