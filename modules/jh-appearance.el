;;; appearance --- Summary

;;; Commentary:

;;; Code:
(load-module 'theme)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)


(let ((font (alist-get :font jh/config))
      (font-size (alist-get :font-size jh/config)))
  (set-frame-font (format "%s %s" font font-size)))


(provide 'jh-appearance)
;;; jh-appearance.el ends here
