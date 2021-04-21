(defun modules/window--load (config)
  "Rules for window placement."
  (setq display-buffer-alist
        '(("\\*e?shell\\*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . -1))
          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|helpful .*: .*\\|Messages\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . 0))
          ("\\*Faces\\*"
           (display-buffer-in-side-window)
           (side . bottom)
           (slot . 1))
          ("Dired by name"
           (display-buffer-in-side-window)
           (slot . 0)
           (side . left)
           (window-width . 0.2)
           (window-parameters . ((no-other-window . t)
                                 (no-delete-other-windows . t)
                                 (mode-line-format . (" " "%b"))))))))
