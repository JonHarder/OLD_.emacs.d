;;; appearance --- Summary

;;; Commentary:

;;; Code:
(require 'seq)

(defun jh/pull-theme (theme &optional theme-package)
  "Download THEME-PACKAGE if THEME is not a built in theme."
  (let* ((theme-name (symbol-name theme))
         (theme-package (if theme-package theme-package (intern (format "%s-theme" theme-name)))))
    (when (not (memq theme (custom-available-themes)))
      (straight-use-package theme-package))))


(defun jh/load-theme (theme)
  "Load THEME, disabling other enabled themes."
  (let* ((theme-name (symbol-name theme))
         (other-themes (seq-filter (lambda (other-theme)
                                    (not (string-equal theme-name other-theme)))
                       custom-enabled-themes)))
    (mapc 'diable-theme other-themes)
    (load-theme theme t)))


(defun jh/set-theme (theme &optional theme-package)
  "Enable THEME, optionally found in THEME-PACKAGE.

 If theme is not a built in theme, and not present on the machine, it will be installed."
  (interactive)
  (jh/pull-theme theme theme-package)
  (jh/load-theme theme)
  (when (functionp 'doom-themes-org-config)
    (doom-themes-org-config)))


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

  (when (alist-get :highlight-line config nil)
    (add-hook 'prog-mode-hook 'hl-line-mode))

  (use-package rainbow-delimiters
    :config
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

  (let ((color-theme (alist-get :color-theme config))
        (color-theme-package (alist-get :color-theme-package config))
        (font (alist-get :font config))
        (font-size (alist-get :font-size config)))
    (jh/set-theme color-theme color-theme-package)
    (set-frame-font (format "%s %s" font font-size))))

(provide 'jh-appearance)
;;; jh-appearance.el ends here
