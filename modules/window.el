(defun modules/window--load (config)
  "Rules for window placement using CONFIG."

  (defun buffer-move-out-of-side-window ()
    (interactive)
    (let ((buffer (current-buffer)))
      (with-current-buffer buffer
        (delete-window)
        (display-buffer-at-bottom
         buffer `((window-parameters . ((mode-line-format . (" " "%b")))))))
      (select-window (get-buffer-window buffer))))

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
