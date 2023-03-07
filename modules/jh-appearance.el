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

(puthash "atom"
         (make-theme
          :package 'atom-one-dark-theme
          :light 'atom-one-dark
          :dark 'atom-one-dark)
         jh/themes)
(puthash "ef-duo"
         (make-theme
          :package 'ef-themes
          :light 'ef-duo-light
          :dark 'ef-duo-dark)
         jh/themes)
(puthash "ef-light-dark"
         (make-theme
          :package 'ef-themes
          :light 'ef-light
          :dark 'ef-dark)
         jh/themes)
(puthash "jetbrains"
         (make-theme
          :package 'jetbrains-darcula-theme
          :light 'jetbrains-darcula
          :dark 'jetbrains-darcula)
         jh/themes)
(puthash "xcode"
         (make-theme
          :package 'doom-themes
          :light 'doom-xcode
          :dark 'doom-xcode)
         jh/themes)
(puthash "horizon"
         (make-theme
          :package 'doom-themes
          :light 'doom-horizon
          :dark 'doom-horizon)
         jh/themes)
(puthash "snazzy"
         (make-theme
          :package 'doom-themes
          :light 'doom-one-light
          :dark 'doom-snazzy)
         jh/themes)
(puthash "rouge"
         (make-theme
          :package 'doom-themes
          :light 'doom-rouge
          :dark 'doom-rouge)
         jh/themes)
(puthash "dark+"
         (make-theme
          :package 'doom-themes
          :light 'doom-dark+
          :dark 'doom-dark+)
         jh/themes)
(puthash "iosvkem"
         (make-theme
          :package 'doom-themes
          :light 'doom-one-light
          :dark 'doom-Iosvkem)
         jh/themes)
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
(puthash "stimmung"
         (make-theme
          :package 'stimmung-themes
          :dark 'stimmung-themes-dark
          :light 'stimmung-themes-light)
         jh/themes)
(puthash "shanty"
         (make-theme
          :package 'shanty-themes
          :dark 'shanty-themes-dark
          :light 'shanty-themes-light)
         jh/themes)

(use-package lin
  :straight (lin :type git :host gitlab :repo "protesilaos/lin")
  :custom
  (lin-mode-hooks '(dired-mode-hook
                    git-rebase-mode-hook
                    ibuffer-mode-hook
                    ilist-mode-hook
                    magit-log-mode-hook
                    notmuch-search-mode-hook
                    occur-mode-hook
                    org-agenda-mode-hook
                    tabulated-list-mode-hook))
  :config
  (lin-global-mode 1))

(defvar jh/--current-theme nil)

(defun jh/theme-customizations (theme)
  "Perform any theme specific configuration for a given THEME."
  (cond
   ((string-prefix-p "modus-" (symbol-name theme))
    (use-package modus-themes
      :custom
      (modus-themes-slanted-constructs t)
      (modus-themes-diffs 'bg-only)
      (modus-themes-mode-line '())
      (modus-themes-org-blocks 'tinted-background)
      (modus-themes-headings '((t . (rainbow))))
      (modus-themes-markup '(bold italic intense))
      (modus-themes-bold-constructs t)
      (modus-themes-syntax '(alt-syntax))
      (modus-themes-prompts '(intense background))
      (modus-themes-completions '((matches . (extrabold background intense))
                                  (selection . (semibold accented intense))
                                  (popup . (accented))))
      (modus-themes-paren-match '(bold underline intense))))

   ((string-prefix-p "humanoid-" (symbol-name theme))
    (use-package humanoid-themes
      :custom
      (humanoid-themes-comment-bg t)
      (humanoid-themes-arc-bg t)
      (humanoid-themes-org-height nil)
      (humanoid-themes-org-agenda-hight t)
      (humanoid-themes-comment-light nil)
      (humanoid-themes-org-highlight t)))

   ((string-prefix-p "kaolin-" (symbol-name theme))
    (use-package kaolin-themes
      :custom
      (kaolin-themes-comments-style 'alt)))

   ((string-prefix-p "spacemacs-" (symbol-name theme))
    (use-package spacemacs-theme
      :custom
      (spacemacs-theme-org-highlight t)
      (spacemacs-theme-comment-italic nil)))

   ;;; doom themes
   ((string-prefix-p "doom-" (symbol-name theme))
    (use-package doom-themes
      :config
      ;; (doom-themes-org-config)
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
  (if jh/MAC-P
    (string-equal "Dark"
                  (string-trim
                   (shell-command-to-string
                    "defaults read -g AppleInterfaceStyle")))
    t))


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
      (message "disabling other themes: %s" other-themes)
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
  (let ((current-theme (if (jh/mac-is-dark-mode-p)
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
  (osx/run-script "toggle-dark-mode.applescript")
  (reload-theme)
  (normal-mode))

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

(general-define-key
 :states 'normal
 :prefix "SPC"
 "c t" #'high-contrast-toggle)

(use-package rainbow-delimiters
  :hook ('prog-mode-hook . rainbow-delimiters-mode))
  ;; :config
  ;; (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(if (display-graphic-p)
    (select-theme jh/theme)
  (select-theme "modus"))

(let ((font (format "%s %s" jh/font jh/font-size)))
  (add-to-list 'default-frame-alist `(font . ,font))
  (set-face-attribute 'variable-pitch nil :family jh/font)
  (set-face-attribute 'default nil :family jh/font))


(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(general-define-key
 :states 'normal
 :prefix "SPC"
 "c h" #'hidden-mode-line-mode)

(provide 'jh-appearance)
;;; jh-appearance.el ends here
