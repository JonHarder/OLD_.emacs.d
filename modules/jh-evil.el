;;; evil --- Summary
;; My configuration for setting up the vim emulation layer within Emacs.

;;; Commentary:
;; For the most part, this just gets evil, sets some settings, and sets up magit to
;; make use of vim bindings

;;; Code:
;; Vim emulation
(use-package evil
  :straight t
  :init
  (setq evil-search-module 'evil-search
        evil-ex-complete-emacs-commands t
        evil-vsplit-window-right nil
        evil-split-window-below nil
        evil-shift-round nil
        evil-want-Y-yank-to-eol t
        evil-want-C-u-scroll t
        evil-want-integration t
        evil-want-keybinding nil
        evil-want-minibuffer nil)

  :config
  (add-hook 'org-mode-hook
      (lambda ()
        (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)))
  (evil-mode 1))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))


(use-package evil-magit
  :straight t)


(use-package evil-org
  :straight t
  :after org
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . evil-org-set-key-theme))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init))


(use-package parinfer
  :straight t
  :init
  (setq parinfer-extensions
    '(defaults
      pretty-parens
      evil))
  (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
  (add-hook 'clojure-mode-hook #'parinfer-mode))


(provide 'jh-evil)
;;; jh-evil.el ends here
