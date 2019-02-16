;;; appearance --- Summary


;;; Commentary:

;;; Code:

(set-frame-font (format "%s %i" jh/font jh/font-size) nil t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(defvar jh/color-themes nil "The available color themes, use in conjunction with jh/set-color-theme.")

(defconst system-themes '("adwaita"
			  "deeper-blue"
			  "dichromacy"
			  "leuven"
			  "light-blue"
			  "manoj-dark"
			  "misterioso"
			  "tango"
			  "tango-dark"
			  "tsdh-light"
			  "tsdh-dark"
			  "wheatgrass"
			  "whiteboard"
			  "wombat"))


(setq jh/color-themes
      (append system-themes
	      '("solarized-dark"
		"solarized-light"
		"spacemacs-dark"
		"spacemacs-light"
		"cyberpunk"
		"rebecca"
		"gruvbox-dark-hard"
		"gruvbox-dark-medium"
		"gruvbox-dark-soft"
		"gruvbox-light-hard"
		"gruvbox-light-medium"
		"gruvbox-light-soft"
		"material"
		"material-light"
		"challenger-deep"
		"zenburn"
		"tango-plus")))


(defun jh/set-color-theme (theme)
  "Set the color theme to THEME."
  (interactive (list
                (completing-read "Theme: " jh/color-themes)))
  (disable-theme (car custom-enabled-themes))
  (setq jh/color-theme theme)
  (cond
   ((member jh/color-theme system-themes)
    (load-theme (intern jh/color-theme) t))
   ((string-match (rx bol "gruvbox" (zero-or-more anything)) jh/color-theme)
    (use-package gruvbox-theme
      :ensure t
      :config
      (load-theme (intern jh/color-theme) t)))
   ((string-match (rx bol "material" (zero-or-more anything)) jh/color-theme)
    (use-package material-theme
      :ensure t
      :config
      (load-theme (intern jh/color-theme) t)))
   ((string-match (rx bol "doom-" (zero-or-more anything)) jh/color-theme)
    (use-package doom-themes
      :ensure t
      :config
      (load-theme (intern jh/color-theme) t)))
   ((member jh/color-theme '("solarized-light" "solarized-dark"))
    (use-package solarized-theme
      :ensure t
      :config
      (load-theme (intern jh/color-theme) t)))
   ((string-equal jh/color-theme "dracula")
    (use-package dracula-theme
      :ensure t
      :config
      (load-theme 'dracula t)))
   ((string-equal jh/color-theme "cyberpunk")
    (use-package cyberpunk-theme
      :ensure t
      :config
      (load-theme 'cyberpunk t)))
   ((string-equal jh/color-theme "monokai")
    (use-package monokai-theme
      :ensure t
      :config
      (load-theme 'monokai t)))
   ((string-equal jh/color-theme "rebecca")
    (use-package rebecca-theme
      :ensure t
      :config
      (load-theme 'rebecca t)))
   ((string-equal jh/color-theme "challenger-deep")
    (use-package challenger-deep-theme
      :ensure t
      :config
      (load-theme 'challenger-deep t)))
   ((string-equal jh/color-theme "tango-plus")
    (use-package tango-plus-theme
      :ensure t
      :config
      (load-theme 'tango-plus t)))
   ((string-equal jh/color-theme "zenburn")
    (use-package zenburn-theme
      :ensure t
      :config
      (load-theme (intern jh/color-theme) t)))
   ((member jh/color-theme '("spacemacs-dark" "spacemacs-light"))
    (use-package spacemacs-theme
      :defer t
      :init (load-theme 'spacemacs-dark t)))))


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

(jh/set-color-theme jh/color-theme)

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/moe-theme-20180617.200/")
;; (add-to-list 'load-path "~/.emacs.d/elpa/moe-theme-20180617.200/")
;; (require 'moe-theme)
;; (load-theme 'moe-dark t)


(provide 'appearance)
;;; appearance.el ends here
