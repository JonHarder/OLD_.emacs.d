;;; package --- Summary

;;; Commentary:

;;; Code:
(require 'bootstrap "~/.emacs.d/bootstrap.el")
(require 'evil)


(defun jh/simple-modeline-segment-tab-name ()
  "Segment to display the current tabs name."
  (let* ((tab-name (string-trim (cdadr (tab-bar--current-tab))))
         (tab-string
          (propertize
           (concat " " tab-name " ")
           'face
           `(:foreground "#1892f0" :background "white" :weight bold))))
    (concat " " tab-string)))

(defun jh/simple-modeline-segment-evil-state ()
  "Segment to display which evil state the buffer is in."
  (let* ((state-properties-alist '((normal . "#55ff55")
                                   (insert . "#ff55ff")
                                   (visual . "#88aaff")))
         (state-color (alist-get evil-state state-properties-alist "white")))
    (concat
     " "
     (propertize
      (capitalize (symbol-name evil-state))
      'face `(:background ,state-color :foreground "black" :weight bold)))))

(defun jh/simple-modeline-segment-file-pct ()
  "Compute the percentage of the file the point is."
  (number-to-string
   (round
    (* 100 (/ (float (point))
              (point-max))))))

(defun modules/modeline--load (config)
  "Load configuration for the modeline using CONFIG."
  (require 'subr-x)

  (setq display-time-day-and-date t)
  (display-time-mode)

  (use-package simple-modeline
    :hook (after-init . simple-modeline-mode)
    :config
    (setq simple-modeline-segments
          '(;; left aligned segments
            (jh/simple-modeline-segment-evil-state
             jh/simple-modeline-segment-tab-name
             simple-modeline-segment-modified
             simple-modeline-segment-buffer-name
             simple-modeline-segment-position
             jh/simple-modeline-segment-file-pct)

            ;; right aligned segments
            (simple-modeline-segment-input-method
             simple-modeline-segment-vc
             simple-modeline-segment-misc-info
             simple-modeline-segment-major-mode)))))
(provide 'modeline)
;;; modeline.el ends here
