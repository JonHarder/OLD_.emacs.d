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
         (state-color (alist-get evil-state state-properties-alist "white"))
         (state-str (cond
                     ((eq 'visual evil-state)
                      (string-trim (evil-visual-message) "[ -]+" "[ -]+"))
                     (t (symbol-name evil-state)))))
    (concat
     " "
     (propertize
       (capitalize state-str)
       'face `(:background ,state-color :foreground "black" :weight bold)))))

(defun jh/simple-modeline-segment-file-pct ()
  "Compute the percentage of the file the point is."
  (let ((pct (round
              (* 100 (/ (float (point))
                        (point-max))))))
    (concat
     " "
     (propertize (format "%dpct" pct) 'face '(:inverse-video t)))))

(require 'battery)

(defun jh/simple-modeline-segment-battery ()
  "Modeline segment responsible for batter information.

Makes use of `battery-status-function' for retrieving information, and
`battery-mode-line-format' to format."
  (let ((jh/battery-format " [bat: %B %p, t-%t] "))
    ;; (battery-format battery-mode-line-format (funcall battery-status-function))
    (battery-format jh/battery-format (funcall battery-status-function))))

(defun jh/simple-modeline-segment-word-count ()
  "Display the number of words in the buffer."
  (concat " "
          (propertize (format "%i words" (count-words (point-min) (point-max)))
            'face `(:underline t))
          " "))


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
             simple-modeline-segment-major-mode
             simple-modeline-segment-modified
             simple-modeline-segment-buffer-name
             simple-modeline-segment-position
             jh/simple-modeline-segment-file-pct
             jh/simple-modeline-segment-word-count)

            ;; right aligned segments
            (simple-modeline-segment-input-method
             simple-modeline-segment-vc
             jh/simple-modeline-segment-battery
             simple-modeline-segment-misc-info)))))

(provide 'modeline)
;;; modeline.el ends here
