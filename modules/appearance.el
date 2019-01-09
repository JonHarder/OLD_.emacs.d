;;; appearance --- Summary


;;; Commentary:

;;; Code:

(set-frame-font "Fira Code 14" nil t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)


(if (equal jh/color-theme "solarized")
    (use-package solarized-theme
      :ensure t
      :config
      (load-theme 'solarized-dark t)))


(defun jh/set-font-size (size)
  "Set the frame font to SIZE."
  (set-frame-font (format "%s %i" jh/font size))
  (setq jh/font-size size))

(defun jh/increse-font-size ()
  "Incerase the font size."
  (interactive)
  (jh/set-font-size (+ 1 jh/font-size)))

(defun jh/decrease-font-size ()
  "Incerase the font size."
  (interactive)
  (jh/set-font-size (- jh/font-size 1)))



(provide 'appearance)
;;; appearance.el ends here
