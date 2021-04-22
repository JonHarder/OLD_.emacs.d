(defun modules/modeline--load (config)
  "Load configuration for the modeline using CONFIG."
  (setq display-time-day-and-date t)
  (display-time-mode)

  (defun jh/simple-modeline-segment-tab-name ()
    (let ((tab-name (cdadr (tab-bar--current-tab))))
      (propertize tab-name 'face `(:foreground "white" :weight bold))))

  (use-package simple-modeline
    :hook (after-init . simple-modeline-mode)
    :config
    (setq simple-modeline-segments '(;; left aligned segments
                                     (jh/simple-modeline-segment-tab-name
                                      simple-modeline-segment-modified
                                      simple-modeline-segment-buffer-name
                                      simple-modeline-segment-position)

                                     ;; right aligned segments
                                     (simple-modeline-segment-input-method
                                      simple-modeline-segment-vc
                                      simple-modeline-segment-misc-info
                                      simple-modeline-segment-major-mode)))))
  ;; (use-package doom-modeline
  ;;   :init
  ;;   (setq doom-modeline-major-mode-color-icon t)
  ;;   :hook (after-init . doom-modeline-mode))
  
