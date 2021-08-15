;;; evil --- All the vim goodness, but in emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;;;; imports
(require 'general)

;;;; code
(use-package evil
  :hook (emacs-startup . evil-mode)
  :init
  (setq evil-search-module 'evil-search
        evil-ex-complete-emacs-commands t
        evil-vsplit-window-right nil
        evil-split-window-below nil
        evil-echo-state nil
        evil-shift-round nil
        evil-want-Y-yank-to-eol t
        evil-want-C-u-scroll nil
        evil-want-integration t
        evil-want-keybinding nil
        evil-want-minibuffer nil)

  :config
  ;; (evil-mode 1)
  (evil-set-initial-state 'magit-status-mode 'normal)
  (evil-set-initial-state 'ibuffer-mode 'normal))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode 1)
  :general
  (:states '(normal visual)
   "gc" 'evil-commentary
   "gy" 'evil-commentary-yank))
  
  
(use-package vi-tilde-fringe
  :hook (prog-mode . vi-tilde-fringe-mode))
  
  
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))
  
  
(use-package evil-org
  :commands (org-agenda-list)
  :after org
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . evil-org-set-key-theme))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :general
  (:keymaps 'org-agenda-mode-map
   :states 'motion
   "l" #'org-agenda-later
   "h" #'org-agenda-earlier))

