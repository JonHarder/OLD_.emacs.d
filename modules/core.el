;;; core --- Basic editing experience configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; This sets up the basics of the configuration and basic settings
;;; (or things that I can't find a better place to put them into)


;;; Code:
(require 'ediff)
(require 'epg-config)
(require 'general)

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

(defun jh/evil-yank-advice (orig-fn beg end &rest args)
  (pulse-momentary-highlight-region beg end)
  (apply orig-fn beg end args))

(advice-add 'evil-yank :around 'jh/evil-yank-advice)

 ;;; minibuffer config
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq dired-listing-switches "-alh")
(setq ring-bell-function 'ignore)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling)
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq custom-file (expand-file-name "custom.el" temporary-file-directory))

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

(defvar apropos-do-all t)
(setq after-focus-change-function #'garbage-collect)

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

 ;;; use internal emacs text browser
 ;; (setq browse-url-browser-function #'eww)
 ;;; Let the system default for osx determine which browser to use
(setq ;; browse-url-browser-function #'browse-url-generic
      browse-url-generic-program "open"
      browse-url-browser-function 'eww
      url-cookie-trusted-urls '()
      url-cookie-untrusted-urls '(".*"))

(setq confirm-kill-processes nil)

(use-package server
  :defer 5
  :config
  (when (not (server-running-p))
    (server-start)))

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package nginx-mode
  :commands nginx-mode)

(use-package emacs-everywhere
  :commands emacs-everywhere)

;; (use-package dash)

(use-package ag
  :commands ag)

(use-package helpful)

(save-place-mode 1)

(use-package rg
  :commands rg
  :config
  (rg-enable-menu))

(use-package scratch
  :commands scratch
  :general
  (:states 'normal
   :prefix "SPC"
   "a s" 'scratch))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

(use-package fireplace
  :commands fireplace)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(require 'uniquify)

(setq uniquify-buffer-name-style 'forward)

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit))

 ;;; Fix all my spelling mistakes
(add-hook 'text-mode-hook #'flyspell-mode)

;; (use-package yasnippet
;;   :custom
;;   (yas-snippet-dirs '("~/.emacs.d/snippets/"))
;;   :config
;;   (yas-global-mode 1)
;;   (defun yas-php-get-class-name-by-file-name ()
;;     (let ((fname (buffer-file-name)))
;;       (file-name-base fname))))

(use-package page-break-lines
  :config
  (global-page-break-lines-mode 1))

(defvar org-publish-project-alist
      '(("Bethlehem Application" .
         (:base-directory "~/Documents/Bethlehem/application"
          :publishing-function org-latex-publish-to-pdf
          :publishing-directory "~/Documents/Bethlehem/application_published"
          :exclude "outline.org"))))

(use-package occur
  :straight nil
  :general
  (:states 'normal
   :keymaps 'occur-mode-map
   "j" 'next-line
   "k" 'previous-line
   "RET" 'occur-mode-goto-occurrence)
  :config
  (evil-define-key 'normal 'occur-mode-map [mouse-2] 'occur-mode-mouse-goto))

(use-package avy
  :commands avy-goto-word-1
  :custom
  (avy-background t)
  :general
  (:states 'normal
   :prefix "SPC"
   "SPC" 'avy-goto-word-1))

(use-package autorevert
  :hook (emacs-startup . global-auto-revert-mode)
  :custom
  (auto-reverse-verbose nil)
  (global-auto-revert-non-file-buffers t))

(use-package abbrev
  :straight nil
  :hook (emacs-startup . read-abbrev-file)
  :custom
  (abbrev-file-name (concat user-emacs-directory "abbrev_defs"))
  (save-abbrevs 'silent)
  :init
  (setq-default abbrev-mode t))

(use-package writeroom-mode
  :commands writeroom-mode
  :custom
  (writeroom-width 0.6)
  :general
  (:states 'normal
   :prefix "SPC"
   "a w" '(writeroom-mode :wk "Writing Mode")))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "pandoc"))
(provide 'core)
;;; core.el ends here
