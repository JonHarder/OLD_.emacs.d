;;; appearance --- Summary

;;; Commentary:

;;; Code:
(load-module 'theme)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)


(jh/set-theme "light")

(let ((enable-theme-switch (plist-get jh/config :enable-theme-switch)))
  (if enable-theme-switch
      (jh/enable-theme-switcher)
     (jh/disable-theme-switcher)))

(provide 'jh-appearance)
;;; jh-appearance.el ends here
