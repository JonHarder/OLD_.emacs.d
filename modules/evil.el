(defun modules/evil--load (config)
  "Some stuff with CONFIG."
  (use-package evil
    :demand t
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

  (use-package evil-commentary
    :config
    (evil-commentary-mode 1))
  
  
  (use-package vi-tilde-fringe
    :defer 3
    :hook (prog-mode . vi-tilde-fringe-mode))
  
  
  (use-package evil-surround
    :defer 1
    :config
    (global-evil-surround-mode 1))
  
  
  (use-package evil-magit
    :demand t)
  
  
  (use-package evil-org
    :defer 1
    :after org
    :hook ((org-mode . evil-org-mode)
           (evil-org-mode . evil-org-set-key-theme))
    :config
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))
  
  
  (use-package evil-collection
    :after evil
    :demand t
    :config
    (evil-collection-init)))
