;;; window --- Window display settings

;;; Commentary:
;;; This is mostly a wrapper around configuring `display-buffer-alist'

;;; Code:
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
           (window-height . 0.4)
           (side . bottom)
           (slot . -1))
          ("\\*info\\*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . right)
           (window-width . 0.4)
           (slot . 0))
          ("\\*ielm\\*"
           (display-buffer-below-selected))
          ("\\*vterminal*\\*"
           (display-buffer-below-selected))
          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|compilation\\|Messages\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.3)
           (side . bottom)
           (slot . 1))
          ("\\*\\(helpful .*: .*\\|Help\\)\\*"
           (display-buffer-in-side-window display-buffer-below-selected)
           (side . bottom)
           (window-height 0.2)))))

(provide 'window)
;;; window.el ends here
