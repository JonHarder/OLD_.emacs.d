;;; appearance -- Configured the visual appearance of emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
;; (require 'seq)
;; (require 'use-package)
(require 'contrib "~/.emacs.d/contrib.el")
(require 'straight)


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
      :dark doom-solarized-dark-high-contrast))
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
      :light doom-dracula))
    ("oceanic" .
     (:package doom-themes
      :dark doom-oceanic-next
      :light doom-tomorrow-day))
    ("nord" .
     (:package doom-themes
      :dark doom-nord
      :light doom-nord-light))
    ("moonlight" .
     (:package doom-themes
      :dark doom-moonlight
      :light doom-one-light))
    ("tomorrow" .
     (:package doom-themes
      :dark doom-tomorrow-night
      :light doom-tomorrow-day))))

(defvar jh/--current-theme nil)

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
      (modus-themes-completions nil)
      (modus-themes-completions 'opinionated)
      ;; (modus-themes-completions 'moderate)
      ;; (modus-themes-scale-headings nil)
      (modus-themes-paren-match 'intense)))

   ;;; doom themes
   ((string-prefix-p "doom-" (symbol-name theme))
    (use-package doom-themes
      :config
      (doom-themes-org-config)
      ;; (doom-themes-enable-org-fontification)
      :custom
      (doom-themes-enable-bold t)
      (doom-themes-enable-italic t)
      (doom-themes-padded-modeline nil)
      (doom-one-light-brighter-comments t)
      (doom-one-brighter-comments t)
      (doom-tomorrow-night-padded-modeline t)
      (doom-solarized-light-brighter-comments t)
      (doom-solarized-dark-brighter-text t)
      (doom-solarized-dark-brighter-modeline t)
      (doom-solarized-dark-brighter-comments t)
      (doom-solarized-dark-high-contrast-brighter-modeline t)
      (doom-solarized-dark-high-contrast-brighter-comments t)))))


(defun jh/mac-is-dark-mode-p ()
  "Determine if MacOS dark theme is enabled."
  (interactive)
  (string-equal "Dark" (string-trim (shell-command-to-string "defaults read -g AppleInterfaceStyle"))))

(defvar jh/dark-mode (jh/mac-is-dark-mode-p) "Boolean which tracks mac system level dark mode.")

(defun jh/set-theme (theme)
  "Load THEME, disabling other enabled themes.

Low level theme manipulation.  Use `select-theme' for higher level
function to load a particular theme."
  (let ((other-themes (seq-filter (lambda (other-theme)
                                    (not (string-equal theme other-theme)))
                                  custom-enabled-themes)))
    (unless (memq theme custom-enabled-themes)
      (jh/theme-customizations theme)
      (load-theme theme t)
      (mapc 'disable-theme other-themes))))


(defun jh/load-theme (theme package)
  "Load THEME, after installing PACKAGE if not found on system."
  (unless (memq theme custom-known-themes)
    (straight-use-package package))
  (jh/set-theme theme))

(defun jh/theme-property (prop)
  "Get a keyword PROP from the theme configuration."
  (plist-get jh/--current-theme prop))

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
  (setq jh/--current-theme (alist-get name jh/themes nil nil #'string-equal)
        jh/theme name)
  (let ((current-theme (if (jh/mac-is-dark-mode-p) (jh/theme-dark) (jh/theme-light))))
    (jh/load-theme current-theme (jh/theme-package)))
  (when (called-interactively-p 'interactive)
    (message "Theme set for current session only, modify jh/theme in init.el to set permanently.")))

(defun reload-theme ()
  "Load configuration given the current values of jh/config."
  (interactive)
  (select-theme jh/theme))

(defvar jh/theme-switch-timer nil "Timer used to schedule querying OSX system color preference.")

(require 'whitespace)

(blink-cursor-mode -1)
show-paren-mode 1
(global-prettify-symbols-mode +1)

(use-package diff-hl
  :custom
  (diff-hl-draw-boarders t)
  (diff-hl-side 'left)
  :hook ((after-init . global-diff-hl-mode)
         (fundamental-mode . diff-hl-margin-mode)))

(fringe-mode '(8 . 8))
(setq-default fringes-outside-margins nil)
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)
(setq-default overflow-newlines-into-fringe t)

(use-package rainbow-delimiters
  :functions 'rainbow-delimiters-mode-enable
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode-enable))

(select-theme jh/theme)
(let ((font (format "%s %s" jh/font jh/font-size)))

  (when jh/theme-switch-timer
    (cancel-timer jh/theme-switch-timer))
  (setq jh/theme-switch-timer
        (run-with-idle-timer 2 1 #'reload-theme))

  (add-to-list 'default-frame-alist `(font . ,font)))
(provide 'appearance)
;;; appearance.el ends here
