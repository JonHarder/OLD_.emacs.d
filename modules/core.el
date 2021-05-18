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

  (use-package avy)

  (global-auto-revert-mode 1)
  ;;; abbreviations
  (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
  (setq save-abbrevs 'silent)
  (setq-default abbrev-mode t)
  (read-abbrev-file)

  ;;; EXPERIMENTAL
  (straight-use-package 'hyperbole)
  (require 'hyperbole)
  (require 'hyrolo)
  (setq hyrolo-file-list (cons "~/.rolo.otl" (append
                                              (cddr (directory-files "~/Org" t "[^.]"))
                                              (cddr (directory-files "~/notes" t)))))

  (with-eval-after-load 'evil
    (evil-define-key 'normal kotl-mode-map
      (kbd "g g") #'kotl-mode:beginning-of-buffer
      (kbd "g c") #'kotl-mode:goto-cell
      (kbd "> >") #'kotl-mode:demote-tree
      (kbd" < <") #'kotl-mode:promote-tree
      "G" #'kotl-mode:end-of-buffer
      "0" #'kotl-mode:move-beginning-of-line
      "^" #'kotl-mode:move-beginning-of-line
      "$" #'kotl-mode:move-end-of-line
      "l" #'kotl-mode:forward-char
      "h" #'kotl-mode:backward-char
      "j" #'kotl-mode:next-line
      "k" #'kotl-mode:previous-line
      "o" (lambda () (interactive) (kotl-mode:add-cell) (evil-insert-state))
      "e" #'kotl-mode:forward-word
      "w" #'kotl-mode:forward-word
      "b" #'kotl-mode:backward-word
      "A" (lambda ()
            (interactive)
            (kotl-mode:move-end-of-line)
            (evil-insert-state))
      "I" (lambda ()
            (interactive)
            (kotl-mode:move-beginning-of-line)
            (evil-insert-state)))
   (evil-define-key 'insert kotl-mode-map
     (kbd "DEL") #'kotl-mode:delete-backward-char)
   (evil-define-key 'visual kotl-mode-map
     "d" #'kotl-mode:kill-region))
  ;;; END EXPERIMENTAL

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
