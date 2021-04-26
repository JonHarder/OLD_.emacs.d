;;; package --- Summary

;;; Commentary:

;;; Code:
(require 'bootstrap "~/.emacs.d/bootstrap.el")

(defun modules/modeline--load (config)
  "Load configuration for the modeline using CONFIG."
  (require 'subr-x)

  (setq display-time-day-and-date t)
  (display-time-mode)

  (defun jh/simple-modeline-segment-tab-name ()
    (let* ((tab-name (string-trim (cdadr (tab-bar--current-tab))))
           (tab-string
            (propertize
             (concat " " tab-name " ")
             'face
             `(:foreground "#1892f0" :background "white" :weight bold))))
      (concat " " tab-string)))


  (use-package simple-modeline
    :hook (after-init . simple-modeline-mode)
    :config
    (setq simple-modeline-segments
          '(;; left aligned segments
            (jh/simple-modeline-segment-tab-name
             simple-modeline-segment-modified
             simple-modeline-segment-buffer-name
             simple-modeline-segment-position)

            ;; right aligned segments
            (simple-modeline-segment-input-method
             simple-modeline-segment-vc
             simple-modeline-segment-misc-info
             simple-modeline-segment-major-mode)))))
(provide 'modeline)
;;; modeline.el ends here
