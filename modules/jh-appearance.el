;;; appearance -- Configured the visual appearance of emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
;; (require 'seq)
;; (require 'use-package)
(require 'contrib "~/.emacs.d/contrib.el")
(require 'straight)
;; (require 'async)
;; (require 'osx "~/.emacs.d/modules/osx.el")

;;; TODO: turn this into a hash-table
(defvar jh/themes (make-hash-table :test #'equal))

(cl-defstruct theme
  (package nil :type symbol)
  (light nil :type symbol)
  (dark nil :type symbol))

(puthash "zenburn"
         (make-theme
          :package 'doom-themes
          :light 'doom-zenburn
          :dark 'doom-zenburn)
         jh/themes)
(puthash "monokai"
         (make-theme
          :package 'doom-themes
          :light 'doom-monokai-pro
          :dark 'doom-monokai-pro)
         jh/themes)
(puthash "humanoid"
         (make-theme
          :package 'humanoid-themes
          :light 'humanoid-light
          :dark 'humanoid-dark)
         jh/themes)
(puthash "acario"
         (make-theme
          :package 'doom-themes
          :light 'doom-acario-light
          :dark 'doom-acario-dark)
         jh/themes)
(puthash "modus"
         (make-theme
          :package 'modus-themes
          :light 'modus-operandi
          :dark 'modus-vivendi)
         jh/themes)
(puthash "spacemacs"
         (make-theme
          :package 'spacemacs-theme
          :light 'spacemacs-light
          :dark 'spacemacs-dark)
         jh/themes)
(puthash "solarized"
         (make-theme
          :package 'doom-themes
          :light 'doom-solarized-light
          :dark 'doom-solarized-dark)
         jh/themes)
(puthash "gruvbox"
         (make-theme
          :package 'doom-themes
          :light 'doom-gruvbox-light
          :dark 'doom-gruvbox)
         jh/themes)
(puthash "doom"
         (make-theme
          :package 'doom-themes
          :light 'doom-one-light
          :dark 'doom-one)
         jh/themes)
(puthash "dracula"
         (make-theme
          :package 'doom-themes
          :dark 'doom-dracula
          :light 'doom-dracula)
         jh/themes)
(puthash "oceanic"
         (make-theme
          :package 'doom-themes
          :dark 'doom-oceanic-next
          :light 'doom-tomorrow-day)
         jh/themes)
(puthash "nord"
         (make-theme
          :package 'doom-themes
          :dark 'doom-nord
          :light 'doom-nord-light)
         jh/themes)
(puthash "moonlight"
         (make-theme
          :package 'doom-themes
          :dark 'doom-moonlight
          :light 'doom-one-light)
         jh/themes)
(puthash "tomorrow"
         (make-theme
          :package 'doom-themes
          :dark 'doom-tomorrow-night
          :light 'doom-tomorrow-day)
         jh/themes)
(puthash "outrun-electric"
         (make-theme
          :package 'doom-themes
          :dark 'doom-outrun-electric
          :light 'doom-one-light)
         jh/themes)
(puthash "material"
         (make-theme
          :package 'doom-themes
          :dark 'doom-material
          :light 'doom-one-light)
         jh/themes)
(puthash "kaolin"
         (make-theme
          :package 'kaolin-themes
          :dark 'kaolin-valley-dark
          :light 'kaolin-valley-light)
         jh/themes)
(puthash "twilight"
         (make-theme
          :package 'twilight-anti-bright-theme
          :dark 'twilight-anti-bright
          :light 'twilight-anti-bright)
         jh/themes)
(puthash "cyberpunk"
         (make-theme
          :package 'cyberpunk-theme
          :dark 'cyberpunk
          :light 'cyberpunk)
         jh/themes)

(defvar jh/--current-theme nil)

(defun jh/theme-customizations (theme)
  "Perform any theme specific configuration for a given THEME."
  (cond
   ((string-prefix-p "modus-" (symbol-name theme))
    (use-package modus-themes
      :custom
      (modus-themes-slanted-constructs t)
      (modus-themes-mode-line '(borderless accented))
      (modus-themes-org-blocks 'tinted-background)
      (modus-themes-headings '((t . (rainbow background))))
      (modus-themes-bold-constructs t)
      (modus-themes-syntax '())
      (modus-themes-prompts '(intense background))
      (modus-themes-completions '((matches . (extrabold background intense))
                                  (selection . (semibold accented intense))
                                  (popup . (accented))))
      (modus-themes-paren-match '(bold underline intense))))

   ((string-prefix-p "humanoid-" (symbol-name theme))
    (use-package humanoid-themes
      :custom
      (humanoid-comment-bg t)
      (humanoid-org-highlight t)))

   ((string-prefix-p "spacemacs-" (symbol-name theme))
    (use-package spacemacs-theme
      :custom
      (spacemacs-theme-org-highlight t)
      (spacemacs-theme-comment-italic nil)))

   ;;; doom themes
   ((string-prefix-p "doom-" (symbol-name theme))
    (use-package doom-themes
      :config
      (doom-themes-org-config)
      :custom
      (doom-themes-enable-bold t)
      (doom-themes-enable-italic t)
      (doom-themes-padded-modeline nil)

      (doom-one-light-brighter-comments t)
      (doom-one-light-brighter-modeline t)

      (doom-one-brighter-comments t)

      (doom-tomorrow-night-padded-modeline t)
      (doom-one-brighter-comments t)
      (doom-one-brighter-modeline t)

      (doom-vibrant-brighter-comments t)
      (doom-vibrant-brighter-modeline t)

      (doom-solarized-light-brighter-comments t)
      (doom-solarized-light-brighter-modeline t)

      (doom-solarized-dark-brighter-text t)
      (doom-solarized-dark-brighter-modeline t)
      (doom-solarized-dark-brighter-comments t)

      (doom-solarized-dark-high-contrast-brighter-modeline t)
      (doom-solarized-dark-high-contrast-brighter-comments t)
      (doom-outrun-electric-brighter-text t)

      (doom-oceanic-next-comment-bg t)
      (doom-oceanic-next-brighter-comments t)
      (doom-oceanic-next-brighter-modeline t)

      (doom-outrun-electric-brighter-modeline t)
      (doom-outrun-electric-brighter-comments t)))))

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

(defun select-theme (name)
  "Select the given theme, indexed by NAME.

Uses the dark or light variant depending on system setting."
  (interactive (list (completing-read
                      "Theme: "
                      (hash-table-keys jh/themes))))
  (setq jh/--current-theme (gethash name jh/themes)
        jh/theme name)
  (let ((current-theme (if jh/dark-mode
                           (theme-dark jh/--current-theme)
                         (theme-light jh/--current-theme))))
    (jh/load-theme current-theme (theme-package jh/--current-theme)))
  (when (called-interactively-p 'interactive)
    (message "Theme set for current session only, modify jh/theme in init.el to set permanently.")))


(defun reload-theme ()
  "Load configuration given the current values of jh/config."
  (interactive)
  (select-theme jh/theme))

(defun osx/toggle-dark-mode ()
  "Toggle OSX dark mode."
  (interactive)
  (setq jh/dark-mode (not jh/dark-mode))
  (reload-theme)
  (normal-mode)
  (osx/run-script "toggle-dark-mode.applescript"))

(defvar jh/theme-switch-timer nil "Timer used to schedule querying OSX system color preference.")

(require 'whitespace)

(blink-cursor-mode -1)
(show-paren-mode 1)
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

(defvar --high-contrast-p nil)
(defvar --high-contrast-old-theme jh/theme)

(defun high-contrast-toggle nil
  "Toggle high contrast 'modus' theme off and on."
  (interactive)
  (if --high-contrast-p
      (progn
        (setq --high-contrast-p nil
              --high-contrast-old-theme jh/theme)
        (select-theme "modus"))
    (progn
      (setq --high-contrast-p t)
      (select-theme --high-contrast-old-theme))))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(select-theme jh/theme)
(let ((font (format "%s %s" jh/font jh/font-size)))
  (add-to-list 'default-frame-alist `(font . ,font)))
(provide 'jh-appearance)
;;; jh-appearance.el ends here
