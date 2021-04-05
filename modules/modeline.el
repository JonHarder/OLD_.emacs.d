(defun modules/modeline--load (config)
  "Load configuration for the modeline using CONFIG."
  (setq display-time-day-and-date t)
  (display-time-mode)
  (use-package doom-modeline
    :init
    (setq doom-modeline-major-mode-color-icon t)
    :hook (after-init . doom-modeline-mode)))
