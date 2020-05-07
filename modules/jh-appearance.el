;;; appearance --- Summary

;;; Commentary:

;;; Code:
(require 'seq)


(defun jh/pull-theme (theme &optional theme-package)
  "Download THEME-PACKAGE if THEME is not a built in theme."
  (let ((theme-package (if theme-package theme-package (intern (format "%s-theme" theme)))))
    (when (not (memq theme (custom-available-themes)))
      (straight-use-package theme-package))))


(defun jh/theme-config (theme)
  "Perform any theme specific configuration for a given THEME."
  (cond
   ;;; modus themes
   ((string-prefix-p "modus-" theme)
    (progn
      (defvar modus-operandi-theme-bold-constructs t)
      (defvar modus-operandi-theme-proportional-fonts nil)
      (defvar modus-operandi-theme-scale-headings t)
      (defvar modus-vivendi-theme-bold-constructs t)
      (defvar modus-vivendi-theme-proportional-fonts nil)
      (defvar modus-vivendi-theme-scale-headings t)))
   ;;; doom themes
   ((functionp 'doom-themes-org-config)
    (doom-themes-org-config))))


(defun jh/load-theme (theme)
  "Load THEME, disabling other enabled themes."
  (interactive "sTheme: ")
  (let ((other-themes (seq-filter (lambda (other-theme)
                                    (not (string-equal theme other-theme)))
                                  custom-enabled-themes)))
    (mapc 'disable-theme other-themes)
    (load-theme (intern theme) t)
    (jh/theme-config theme)))


(defun jh/set-theme (theme &optional theme-package)
  "Enable THEME, optionally found in THEME-PACKAGE.

 If theme is not a built in theme, and not present on the machine, it will be installed."
  (interactive "sTheme: ")
  (jh/pull-theme theme theme-package)
  (jh/load-theme theme))


(defun modules/appearance--load (config)
  "Load appearance settings based off of CONFIG."
  (require 'whitespace)

  (blink-cursor-mode -1)
  (show-paren-mode 1)
  (electric-pair-mode 1)
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

  (when (alist-get :highlight-line config nil)
    (add-hook 'prog-mode-hook 'hl-line-mode))

  (use-package rainbow-delimiters
    :config
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

  (let* ((color-theme (alist-get :color-theme config))
         (color-theme-package (alist-get :color-theme-package config))
         (font-name (alist-get :font config))
         (font-size (alist-get :font-size config))
         (font (format "%s %s" font-name font-size)))
    (jh/set-theme (symbol-name color-theme) color-theme-package)
    (set-frame-font font)
    (add-to-list 'default-frame-alist `(font . ,font))))


(provide 'jh-appearance)
;;; jh-appearance.el ends here
