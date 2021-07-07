;;; -*- lexical-binding: t -*-
(defun modules/evil--load (config)
  "Some stuff with CONFIG."
  (use-package evil
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
    (evil-mode 1)
    ;; (add-hook 'kotl-mode-hook #'evil-emacs-state)
    (add-hook 'zpresent-mode-hook #'evil-emacs-state)
    (add-hook 'hyrolo-mode-hook #'evil-emacs-state))

  (use-package evil-collection
    :after evil
    :init
    (setq evil-collection-setup-minibuffer t)
    :config
    (evil-collection-init))
    ;; (evil-collection-define-key 'normal 'minibuffer-local-map
    ;;   (kbd "j") #'selectrum-next-candidate)
    ;; (evil-collection-define-key 'normal 'minibuffer-local-map
    ;;   (kbd "k") #'selectrum-previous-candidate)
    ;; (evil-collection-define-key 'normal 'minibuffer-local-map
    ;;   (kbd "G") #'selectrum-goto-end)
    ;; (evil-collection-define-key 'normal 'minibuffer-local-map
    ;;   (kbd "gg") #'selectrum-goto-beginning)
    ;; (evil-collection-define-key 'insert 'minibuffer-local-map
    ;;   (kbd "C-n") #'selectrum-next-candidate)
    ;; (evil-collection-define-key 'insert 'minibuffer-local-map
    ;;   (kbd "C-p") #'selectrum-previous-candidate))

  (use-package evil-matchit
    :config
    (global-evil-matchit-mode 1))


  (use-package evil-commentary
    :config
    (evil-commentary-mode 1))
  
  
  (use-package vi-tilde-fringe
    :hook (prog-mode . vi-tilde-fringe-mode))
  
  
  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))
  
  
  (use-package evil-org
    :after org
    :hook ((org-mode . evil-org-mode)
           (evil-org-mode . evil-org-set-key-theme))
    :config
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)
    (evil-collection-define-key 'motion 'org-agenda-mode-map (kbd "l") #'org-agenda-later)
    (evil-collection-define-key 'motion 'org-agenda-mode-map (kbd "h") #'org-agenda-earlier)))

