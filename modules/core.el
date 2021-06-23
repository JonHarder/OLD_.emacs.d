;;; core --- Basic editing experience configuration

;;; Commentary:
;;; This sets up the basics of the configuration and basic settings
;;; (or things that I can't find a better place to put them into)


;;; Code:
(require 'use-package)
(require 'ediff)
(require 'epg-config)
(require 'straight)

(defun modules/core--load (config)
  "Load general core features, configure programming hook using CONFIG."
  
  ;;;; from the variable documentation
  ;;; This variable is obsolete since 27.1; use epg-pinentry-mode instead.
  ;; (setq epa-pinentry-mode 'loopback)
  (setq epg-pinentry-mode 'loopback)

  (winner-mode 1)
  (desktop-save-mode 1)

  (defun jh/evil-yank-advice (orig-fn beg end &rest args)
    (pulse-momentary-highlight-region beg end)
    (apply orig-fn beg end args))

  (advice-add 'evil-yank :around 'jh/evil-yank-advice)
  (tab-bar-mode 1)

  ;;; minibuffer config
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

  ;;; use internal emacs text browser
  ;; (setq browse-url-browser-function #'eww)
  ;;; Let the system default for osx determine which browser to use
  (setq browse-url-browser-function #'browse-url-generic
        browse-url-generic-program "open")

  (setq confirm-kill-processes nil)

  (use-package csv-mode)
  (use-package neon-mode)
  (use-package crontab-mode)
  (use-package nginx-mode)

  (straight-use-package 'dash)
  (straight-use-package 'ag)

  (use-package helpful)

  (save-place-mode 1)

  (use-package rg
    :config
    (rg-enable-menu))

  (use-package scratch)


  (straight-use-package 'fireplace)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)


  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

  (use-package origami
    :config
    (global-origami-mode))

  (use-package flycheck
    :config
    (setq-default flycheck-emacs-lisp-load-path 'inherit)
    (add-hook 'after-init-hook #'global-flycheck-mode))

  ;;; Fix all my spelling mistakes
  (add-hook 'text-mode-hook #'flyspell-mode)

  (use-package yasnippet
    :custom
    (yas-snippet-dirs '("~/.emacs.d/snippets/"))
    :config
    (yas-global-mode 1)
    (defun yas-php-get-class-name-by-file-name ()
      (let ((fname (buffer-file-name)))
        (file-name-base fname))))

  ;; (defun autoinsert-yas-expand ()
  ;;     "Replace text in yasnippet template."
  ;;     (yas-expand-snippet (buffer-string) (point-min) (point-max)))
  ;;
  ;; (use-package autoinsert
  ;;   :custom
  ;;   (auto-insert-query nil)
  ;;   (auto-insert-directory (locate-user-emacs-file "templates"))
  ;;   :config
  ;;   (add-hook 'find-file-hook 'auto-insert)
  ;;   (auto-insert-mode 1)
  ;;   ;;; automatically insert php class snippet when opening a new EMPTY php file
  ;;   (define-auto-insert "\\.php$" ["default-php.php" autoinsert-yas-expand]))

  (use-package avy)

  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
   ;;; abbreviations
  (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
  (setq save-abbrevs 'silent)
  (setq-default abbrev-mode t)
  (read-abbrev-file)

  (use-package writeroom-mode)
  
  (use-package markdown-mode
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :custom
    (markdown-command "pandoc")))
(provide 'core)
;;; core.el ends here
