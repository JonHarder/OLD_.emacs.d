(require 'seq)


(defun jh/pull-theme (theme &optional theme-package)
  "Download THEME-PACKAGE if THEME is not a built in theme."
  (let ((theme-package (if theme-package theme-package (intern (format "%s-theme" theme)))))
    (when (not (memq (intern theme) (custom-available-themes)))
      (straight-use-package theme-package))))


(defun jh/theme-config (theme)
  "Perform any theme specific configuration for a given THEME."
  (cond
   ;;; modus themes
   ((string-prefix-p "modus-" theme)
    (progn
      (setq modus-operandi-theme-scale-headings t
            modus-vivendi-theme-scale-headings t
            modus-operandi-theme-bold-constructs t
            modus-vivendi-theme-bold-constructs t
            modus-operandi-theme-proportional-fonts nil
            modus-vivendi-theme-proportional-fonts nil

            modus-operandi-theme-3d-modeline nil
            modus-vivendi-theme-3d-modeline nil

            modus-vivendi-theme-section-headings t
            modus-operandi-theme-section-headings t

            modus-operandi-theme-completions 'opinionated
            modus-vivendi-theme-completions 'opinionated

            modus-operandi-theme-intense-paren-match t
            modus-vivendi-theme-intense-paren-match t

            modus-operandi-theme-rainbow-headings t
            modus-vivendi-theme-rainbow-headings t)))
            
   ;;; doom themes
   ((string-prefix-p "doom-" theme)
    (progn
      (doom-themes-org-config)
      (setq doom-themes-enable-bold t
            doom-themes-enable-italic t
            doom-themes-padded-modeline nil)))))


(defun jh/set-theme (theme)
  "Load THEME, disabling other enabled themes."
  (let ((other-themes (seq-filter (lambda (other-theme)
                                    (not (string-equal theme other-theme)))
                                  custom-enabled-themes)))
    (mapc 'disable-theme other-themes)
    (jh/theme-config theme)
    (load-theme (intern theme) t)))


(defun jh/load-theme (&optional theme theme-package)
  "Enable THEME, optionally found in THEME-PACKAGE.

 If theme is not a built in theme, and not present on the machine, it will be installed."
  (interactive)
  (let ((theme (or theme (completing-read "Theme: " (custom-available-themes)))))
    (jh/pull-theme theme theme-package)
    (jh/set-theme theme)))


(defun modules/appearance--load (config)
  "Load appearance settings based off of CONFIG."
  (require 'whitespace)

  (blink-cursor-mode -1)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (global-prettify-symbols-mode +1)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)

  (defun jh/load-light-theme ()
    (interactive)
    (straight-use-package 'modus-operandi-theme)
    (jh/load-theme "modus-operandi"))

  (defun jh/load-dark-theme ()
    (interactive)
    (straight-use-package 'modus-vivendi-theme)
    (jh/load-theme "modus-vivendi"))

  (use-package diff-hl
    :config
    (setq diff-hl-draw-borders nil)
    (setq diff-hl-side 'left)
    :hook ((after-init . global-diff-hl-mode)))

  (fringe-mode '(8 . 8))
  (setq-default fringes-outside-margins nil)
  (setq-default indicate-buffer-boundaries nil)
  (setq-default indicate-empty-lines nil)
  (setq-default overflow-newlines-into-fringe t)

  (when (alist-get :highlight-line config nil)
    (add-hook 'prog-mode-hook 'hl-line-mode))

  (use-package rainbow-delimiters
    :config
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

  (let* ((color-theme (alist-get :color-theme config))
         (font-name (alist-get :font config))
         (font-size (alist-get :font-size config))
         (font (format "%s %s" font-name font-size)))
    (cond
     ((eq color-theme 'dark) (jh/load-dark-theme))
     ((eq color-theme 'light) (jh/load-light-theme)))

    (set-frame-font font)
    (add-to-list 'default-frame-alist `(font . ,font))))
