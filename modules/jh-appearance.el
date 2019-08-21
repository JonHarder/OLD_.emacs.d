;;; appearance --- Summary

;;; Commentary:

;;; Code:
(require 'seq)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)


(use-package eyebrowse
  :straight t
  :config
  (eyebrowse-mode t))


(defun jh/set-theme (theme)
  "Enable THEME, disabling all others."
  (interactive)
  (let* ((theme-name (symbol-name (if (listp theme) (car theme) theme)))
         (theme-package (if (listp theme)
                            (cdr theme)
                          (intern (format "%s-theme" theme))))
         (other-themes (seq-filter
                        (lambda (other-theme)
                          (not (string-equal theme-name other-theme)))
                        custom-enabled-themes))
         (theme (intern theme-name)))
    (mapc 'disable-theme other-themes)
    (when (not (memq theme (custom-available-themes)))
      (straight-use-package theme-package))
    (load-theme theme t)))


(jh/set-theme (alist-get :color-theme jh/config))

(let ((font (alist-get :font jh/config))
      (font-size (alist-get :font-size jh/config)))
  (set-frame-font (format "%s %s" font font-size)))


(provide 'jh-appearance)
;;; jh-appearance.el ends here
