;;; core --- Basic editing experience configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; This sets up the basics of the configuration and basic settings
;;; (or things that I can't find a better place to put them into)


;;; Code:
(require 'ediff)
(require 'epg-config)
(require 'general)

(defun modules/core--load (config)
  "Load general core features, configure programming hook using CONFIG."
  
  ;;;; from the variable documentation
  ;;; This variable is obsolete since 27.1; use epg-pinentry-mode instead.
  ;; (setq epa-pinentry-mode 'loopback)
  (setq epg-pinentry-mode 'loopback)

  (winner-mode 1)
  ;;; this seems to be freezing everything?
  ;;; plus why would you ever quit emacs?
  ;; (desktop-save-mode 1)

  (use-package emacs
    :custom
    (ns-pop-up-frames nil))

  (use-package quelpa
    :ensure t)

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
        browse-url-generic-program "open"
        url-cookie-trusted-urls '()
        url-cookie-untrusted-urls '(".*"))

  (setq confirm-kill-processes nil)

  (use-package csv-mode
    :ensure t
    :mode "\\.csv\\'")

  (use-package nginx-mode
    :commands nginx-mode
    :ensure t)

  (use-package dash
    :ensure t)

  (use-package ag
    :commands ag
    :ensure t)

  (use-package helpful
    :ensure t)

  (save-place-mode 1)

  (use-package rg
    :ensure t
    :commands rg
    :config
    (rg-enable-menu))

  (use-package scratch
    :ensure t
    :commands scratch
    :general
    (:states 'normal
     :prefix "SPC"
     "a s" 'scratch))

  (use-package undo-tree
    :ensure t
    :config
    (global-undo-tree-mode 1))

  (use-package fireplace
    :ensure t
    :commands fireplace)

  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

  (require 'uniquify)

  (setq uniquify-buffer-name-style 'forward)

  (use-package flycheck
    :ensure t
    :config
    (setq-default flycheck-emacs-lisp-load-path 'inherit)
    (add-hook 'after-init-hook #'global-flycheck-mode))

  ;;; Fix all my spelling mistakes
  (add-hook 'text-mode-hook #'flyspell-mode)

  (use-package yasnippet
    :ensure t
    :custom
    (yas-snippet-dirs '("~/.emacs.d/snippets/"))
    :config
    (yas-global-mode 1)
    (defun yas-php-get-class-name-by-file-name ()
      (let ((fname (buffer-file-name)))
        (file-name-base fname))))

  (use-package page-break-lines
    :ensure t
    :config
    (global-page-break-lines-mode 1))

  (defvar org-publish-project-alist
        '(("Bethlehem Application" .
           (:base-directory "~/Documents/Bethlehem/application"
            :publishing-function org-latex-publish-to-pdf
            :publishing-directory "~/Documents/Bethlehem/application_published"
            :exclude "outline.org"))))

  (use-package avy
    :ensure t
    :commands avy-goto-word-1)

  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
   ;;; abbreviations
  (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
  (setq save-abbrevs 'silent)
  (setq-default abbrev-mode t)
  (read-abbrev-file)

  (use-package writeroom-mode
    :ensure t
    :commands writeroom-mode
    :custom
    (writeroom-width 0.6)
    :general
    (:states 'normal
     :prefix "SPC"
     "a w" '(writeroom-mode :wk "Writing Mode")))

  (use-package markdown-mode
    :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :custom
    (markdown-command "pandoc")))
(provide 'core)
;;; core.el ends here
