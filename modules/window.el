;;; window --- Window display settings

;;; Commentary:
;;; This is mostly a wrapper around configuring `display-buffer-alist'

;;; Code:
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
         ;; (display-buffer-below-selected)
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
        ("\\*vterm\\*"
         (display-buffer-at-bottom)
         (window-height . 0.4)
         (side . bottom))
        ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|compilation\\|Messages\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.3)
         (side . bottom)
         (slot . 1))
        ("\\*Calendar\\*"
         (display-buffer-at-bottom)
         (wiwndow-height . 0.2)
         (side . bottom))
        ("\\*\\(helpful .*: .*\\|Help\\)\\*"
         (display-buffer-in-side-window display-buffer-below-selected)
         (side . left)
         (window-width . 75))))

(provide 'window)
;;; window.el ends here
