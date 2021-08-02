;;; evil --- All the vim goodness, but in emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;;;; imports
(require 'general)

;;;; code
(use-package evil
  :ensure t
  :demand t
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

  :hook (sunrise-mode-hook . evil-emacs-state)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :demand t
  :init
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-matchit
  :ensure t
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode 1)
  :general
  (:states '(normal visual)
   "gc" 'evil-commentary
   "gy" 'evil-commentary-yank))
  
  
(use-package vi-tilde-fringe
  :ensure t
  :hook (prog-mode . vi-tilde-fringe-mode))
  
  
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
  
  
(use-package evil-org
  :ensure t
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

