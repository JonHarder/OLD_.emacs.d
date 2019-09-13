;;; appearance --- Summary

;;; Commentary:

;;; Code:
(require 'seq)

(defun jh/set-theme (theme &optional theme-package)
  "Enable THEME, optionally found in THEME-PACKAGE if supplied, disabling all others."
  (interactive)
  (let* ((theme-name (symbol-name theme))
         (other-themes (seq-filter
                       (lambda (other-theme)
                         (not (string-equal theme-name other-theme)))
                       custom-enabled-themes))
         (theme-package (if theme-package theme-package (intern (format "%s-theme" theme-name)))))
    (when (not (memq theme (custom-available-themes)))
      (straight-use-package theme-package))
    (mapc 'disable-theme other-themes)
    (load-theme theme t)
    (when (functionp 'doom-themes-org-config)
      (doom-themes-org-config))))


(defun modules/appearance--load (config)
  "Load appearance settings based off of CONFIG."
  (require 'whitespace)

  (blink-cursor-mode -1)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (global-prettify-symbols-mode +1)

  (use-package rainbow-delimiters
    :config
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

  (let ((color-theme (alist-get :color-theme config))
        (color-theme-package (alist-get :color-theme-package config))
        (font (alist-get :font config))
        (font-size (alist-get :font-size config)))
    (menu-bar-mode -1)
    (toggle-scroll-bar -1)
    (tool-bar-mode -1)
    (add-hook 'prog-mode-hook 'hl-line-mode)

    (jh/set-theme color-theme color-theme-package)
    (set-frame-font (format "%s %s" font font-size))))

(provide 'jh-appearance)
;;; jh-appearance.el ends here
