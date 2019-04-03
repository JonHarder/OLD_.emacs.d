;;; appearance --- Summary

;;; Commentary:

;;; Code:
(require 'seq)


(defvar current-theme "dark"
  "The current variant of theme.  Values can be \"dark\" and \"light\".")


(defun jh/set-theme (variant)
  "Set the theme specified by VARIANT to it's dark or light version."
  (interactive (list (completing-read "Theme: " '("light" "dark"))))
  (let* ((light-theme (plist-get jh/config :color-theme-light))
         (dark-theme (plist-get jh/config :color-theme-dark))
         (theme (if (string-equal variant "light") light-theme dark-theme))
         (other-themes (seq-filter (lambda (other-theme) (not (string-equal theme other-theme)))
                          custom-enabled-themes)))
    (mapc 'disable-theme other-themes)
    (cond
     ((string-equal variant "light")
      (when (not (string-equal current-theme "light"))
        (load-theme light-theme t)
        (setq current-theme "light")))
     ((string-equal variant "dark")
      (when (not (string-equal current-theme "dark"))
        (load-theme dark-theme t)
        (setq current-theme "dark"))))))


(defun ensure-lmutracker ()
  "Compile the light sensor script if it's not already."
  (when (not (file-exists-p "~/.emacs.d/external_scripts/lmutracker"))
    (shell-command "bash -c 'cd ~/.emacs.d/external_scripts && clang -o lmutracker lmutracker.cpp -framework IOKit -framework CoreFoundation'")))


(defun ambient-light-reading ()
  "Get the ambient light as recorded by light sensor on laptop."
  (string-to-number
   (shell-command-to-string
    "~/.emacs.d/external_scripts/lmutracker")))


(defconst ambient-light-threshold 3000000
  "The ambient light threshold, below it the color theme will be dark, above it, the theme will be light.")


(defun change-theme-for-lighting ()
  "Switch color themes based on ambient light.

Toggles between the first and second items in the light and dark color themes."
  (let* ((ambient-light (ambient-light-reading))
         (light-threshold ambient-light-threshold))
   (if (< ambient-light light-threshold)
       (jh/set-theme "dark")
     (jh/set-theme "light"))))

(defvar change-theme-func nil "The function to call on an interval to switch between dark and light themes.")

(defun change-theme-for-time-of-day ()
  "Change the color theme between light and dark depending on time of day."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (if (or (< hour 5)
            (> hour 17))
      (jh/set-theme "dark")
     (jh/set-theme "light"))))


(if (eq system-type 'darwin)
    (progn
      (ensure-lmutracker)
      (setq change-theme-func #'change-theme-for-lighting))
  (setq change-theme-func #'change-theme-for-time-of-day))


(defvar theme-switch-timer nil
  "The timer which handles calling the `change-theme-func'.")

(defun jh/enable-theme-switcher ()
  (interactive)
  (setq theme-switch-timer (run-with-timer 0 1 change-theme-func)))

(defun jh/disable-theme-switcher ()
  (interactive)
  (when (and (boundp 'theme-switch-timer)
             (timerp theme-switch-timer))
      (cancel-timer theme-switch-timer)
      (setq theme-switch-timer nil)))


(provide 'jh-theme)
;;; jh-theme.el ends here
