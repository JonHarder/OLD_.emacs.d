(defun modules/window--load (config)
  "Rules for window placement using CONFIG."

  (defun buffer-move-out-of-side-window ()
    (interactive)
    (let ((buffer (current-buffer)))
      (with-current-buffer buffer
        (delete-window)
        (display-buffer-at-bottom
         buffer nil))
      (select-window (get-buffer-window buffer))))

  (setq display-buffer-alist
        '(("\\*\\(e?shell\\)\\*"
           (display-buffer-at-bottom)
           (window-height . 0.3)
           (side . bottom)
           (slot . -1))
          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|compilation\\|Messages\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.3)
           (side . bottom)
           (slot . 0))
          ("\\*\\(helpful .*: .*\\|Help\\)\\*"
           (display-buffer-in-side-window)
           (side . right)
           (window-width . 0.4)))))
