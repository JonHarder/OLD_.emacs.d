(defun modules/window--load (config)
  "Rules for window placement."
  (setq display-buffer-alist
        '(("\\*\\(e?shell\\|vterm\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . -1))
          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|helpful .*: .*\\|Help\\|Messages\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . 0)))))
