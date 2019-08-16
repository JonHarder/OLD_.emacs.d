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
  (let ((other-themes (seq-filter
                       (lambda (other-theme)
                         (not (string-equal theme other-theme)))
                       custom-enabled-themes)))
    (mapc 'disable-theme other-themes)
    (load-theme theme t)))


(jh/set-theme (alist-get :color-theme jh/config))

(let ((font (alist-get :font jh/config))
      (font-size (alist-get :font-size jh/config)))
  (set-frame-font (format "%s %s" font font-size)))


(provide 'jh-appearance)
;;; jh-appearance.el ends here
