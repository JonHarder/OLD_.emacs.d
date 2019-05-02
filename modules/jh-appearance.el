;;; appearance --- Summary

;;; Commentary:

;;; Code:
(load-module 'theme)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)


(let ((default-variant (plist-get jh/config :color-theme-default)))
   (jh/set-theme default-variant))

(let ((enable-theme-switch (plist-get jh/config :enable-theme-switch)))
  (if enable-theme-switch
      (progn
        (message "enabling theme switcher")
        (jh/enable-theme-switcher))
    (progn
      (message "disabling theme switcher")
      (jh/disable-theme-switcher))))

(provide 'jh-appearance)
;;; jh-appearance.el ends here
