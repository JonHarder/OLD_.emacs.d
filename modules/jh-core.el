;;; core --- Basic editing experience configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; This sets up the basics of the configuration and basic settings
;;; (or things that I can't find a better place to put them into)


;;; Code:
(require 'ediff)
(require 'epg-config)
(require 'general)




(setq auth-sources '("~/.authinfo.gpg"))

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))
    

(add-hook 'after-init-hook
          (lambda ()
            (load "~/.emacs.d/ext_lisp/make.el")))

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

;;; TODO: This is a hack put in place to fix a bad gnu tls implementation
;;;       which was causing eww to hang on almost any https request.
;;;       check back in on this at some point to see if it's still necessary.
(use-package gnutls
  :straight nil
  ;; :demand t
  :disabled t
  :custom
  (gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

(defvar apropos-do-all t)
(setq after-focus-change-function #'garbage-collect)

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(use-package eww
  :straight nil
  :general
  (:states 'normal
   :keymaps 'eww-mode-map
   "H" 'eww-back-url))

 ;;; use internal emacs text browser
 ;; (setq browse-url-browser-function #'eww)
 ;;; Let the system default for osx determine which browser to use
(setq ;; browse-url-browser-function #'browse-url-generic
      browse-url-generic-program "open"
      browse-url-browser-function 'browse-url-default-browser
      url-cookie-trusted-urls '()
      url-cookie-untrusted-urls '(".*"))

(setq confirm-kill-processes nil)

(use-package server
  :defer 5
  :config
  (when (not (server-running-p))
    (server-start)))

(use-package info
  :straight nil
  :general
  (:keymaps 'Info-mode-map
   :states '(normal motion)
   "RET" 'Info-follow-nearest-node
   "n" 'Info-next
   "p" 'Info-prev
   "u" 'Info-up
   "g t" 'Info-menu
   "g T" 'Info-toc
   ", d" 'Info-directory))

(setq browse-url-chrome-program "chrome")

(defvar hugo-server-process nil)

(defun hugo-server-start ()
  "Start a hugo server in `default-directory'."
  (interactive)
  (setq hugo-server-process (start-process "hugo server -D" "*hugo-server*" "hugo" "server" "-D"))
  (let ((display-buffer-alist '(("*" display-buffer-no-window))))
    (async-shell-command "open http://localhost:1313"))
  (message "Server started"))

(defun hugo-server-stop ()
  "Stop the running hugo server tracked in `hugo-server-process'."
  (interactive)
  (if hugo-server-process
      (progn
        (kill-process hugo-server-process)
        (setq hugo-server-process nil)
        (kill-buffer "*hugo-server*")
        (message "server stopped"))
    (message "server not running.")))

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package nginx-mode
  :commands nginx-mode)

(use-package emacs-everywhere
  :commands emacs-everywhere)

;; (use-package dash)

(use-package ag
  :commands ag)

(use-package pdf-tools
  :mode ("\\.pdf" . pdf-view-mode)
  :general
  (:keymaps 'pdf-view-mode-map
   :states '(normal motion)
   "j" 'pdf-view-next-line-or-next-page
   "k" 'pdf-view-previous-line-or-previous-page))


;; (mode-local hide-cursor--original nil)
(define-minor-mode hide-cursor-mode
  "Hide or show the cursor.

When the cursor is hidden `scroll-lock-mode' is enabled, so that
the buffer works like a pager."
  :global nil
  (if hide-cursor-mode
      (progn
        (scroll-lock-mode 1)
        ;;; there seems to be a bug with evil mode not respecting cursor-type,
        ;;; so using `(internal-show-cursor)' instead
        (internal-show-cursor nil nil)
        ;; (setq-local hide-cursor--original cursor-type)
        ;; (setq-local cursor-type nil))
        )
    (scroll-lock-mode -1)
    (internal-show-cursor nil t)
    ;; (setq-local cursor-type (or hide-cursor--original t))
    ))

(use-package doc-view
  :straight nil
  :general
  (:keymaps 'doc-view-mode-map
   :states 'normal
   "j" 'doc-view-next-line-or-next-page
   "k" 'doc-view-previous-line-or-previous-page
   "<up>" 'doc-view-previous-line-or-previous-page
   "<down>" 'doc-view-next-line-or-next-page
   "l" 'doc-view-next-page
   "h" 'doc-view-previous-page
   "<right>" 'doc-view-next-page
   "<left>" 'doc-view-previous-page
   "/" 'doc-view-search
   "+" 'doc-view-enlarge
   "-" 'doc-view-shrink
   "gg" 'beginning-of-buffer
   "G" 'end-of-buffer))


(use-package helpful
  :general
  (:keymaps 'helpful-mode-map
   :states 'normal
   "RET" 'helpful-visit-reference)
  (:states 'normal
   :prefix "SPC"
   "h f" 'helpful-callable
   "h k" 'helpful-key
   "h v" 'helpful-variable))

(save-place-mode 1)

(use-package rg
  :commands rg
  :config
  (rg-enable-menu)
  :general
  (:states '(motion)
   :keymaps 'rg-mode-map
   "j" #'next-error-no-select
   "k" #'previous-error-no-select))

(use-package scratch
  :commands scratch
  :general
  (:states 'normal
   :prefix "SPC"
   "a s" (lambda () (interactive)
           (setq current-prefix-arg '(4))
           (call-interactively #'scratch))))

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

(use-package svg-lib)

(use-package avy
  :commands avy-goto-word-1
  :custom
  (avy-background t)
  :general
  (:states '(normal visual)
   :prefix "SPC"
   "SPC" 'avy-goto-subword-1))

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

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "pandoc")
  :general
  (:keymaps 'markdown-mode-map
   :states 'normal
   :prefix ","
   "p" 'markdown-preview))

(use-package olivetti
  :custom
  (olivetti-style 'fancy)
  :general
  (:states 'normal
   :prefix "SPC"
   "a o" 'olivetti-mode))

(use-package bookmark
  :straight nil
  :general
  (:keymaps 'bookmark-bmenu-mode-map
   :states 'normal
   "RET" 'bookmark-bmenu-this-window
   "r" 'bookmark-bmenu-rename
   "d" 'bookmark-bmenu-delete
   "x" 'bookmark-bmenu-execute-deletions
   "u" 'bookmark-bmenu-unmark
   "o" 'bookmark-bmenu-other-window))

(provide 'jh-core)
;;; jh-core.el ends here
