;;; completion --- Configures completion framework, currently, corfu, vertico -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(use-package corfu
  :init
  (corfu-global-mode 1)
  :custom
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  :general
  (:keymaps 'corfu-map
   :states '(normal insert)
   "TAB" 'corfu-next
   [tab] 'corfu-next
   "S-TAB" 'corfu-previous
   [backtab] 'corfu-previous))

(use-package emacs
  :custom
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  (resize-mini-windows 'grow-only)
  ;; (minibuffer-prompt-properties
  ;;       '(read-only t cursor-intangible t face minibuffer-prompt))
  (enable-recursive-minibuffers t)
  :config
  (minibuffer-depth-indicate-mode 1))
  ;; (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package vertico
  :hook (emacs-startup . vertico-mode)
  :custom
  (vertico-cycle t)
  :config
  (require 'vertico-directory) ;; ~/.emacs.d/ext_lisp/vertico-directory.el
  (general-define-key
   ;; :keymaps 'minibuffer-mode-map
   :keymaps 'vertico-map
   "C-n"   'vertico-next
   "C-p"   'vertico-previous))

(use-package vertico-directory
  :straight nil
  :after vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :general
  (:keymaps 'vertico-map
   "DEL" 'vertico-directory-delete-char
   "M-DEL" 'vertico-directory-delete-word
   "RET" 'vertico-directory-enter))

(use-package orderless
  :custom
  (completion-styles '(orderless substring initials flex partial-completion))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package embark
  :after (vertico marginalia)
  :commands (embark-dwim embark-act embark-prefix-help-command)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :general
  (:keymaps 'override
   "C-." 'embark-dwim
   "M-." 'embark-act))

(use-package recentf
  :hook (emacs-startup . recentf-mode))

(use-package savehist
  :hook (emacs-startup . savehist-mode))

(use-package consult
  :init
  (fset 'multi-occur #'consult-multi-occur)
  :config
  (advice-add #'completing-read-multiple
              :override #'consult-completing-read-multiple)
  :general
  ("C-c h" #'consult-history
   "C-c o" #'consult-outline
   "C-x b" #'consult-buffer)
  (:keymaps 'flycheck-command-map
   :states 'normal
   "!" #'consult-flycheck)
  (:prefix "SPC"
   :states 'normal
   "o o" 'consult-outline
   "b b" 'consult-buffer
   "i i" 'consult-imenu
   "h a" 'consult-apropos
   "l l" 'consult-flycheck
   "/" 'consult-grep))

(use-package consult-dir
  :after (evil)
  :demand t
  :general
  ("C-c C-d" 'consult-dir)
  (:keymaps 'vertico-map
   "C-c C-j" 'consult-dir-jump-file)
  (:states 'normal
   :prefix "SPC"
   "f d" 'consult-dir))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

(use-package xref
  :straight nil
  :general
  (:keymaps 'xref--xref-buffer-mode-map
   :states 'normal
   "j" 'xref-next-line
   "k" 'xref-prev-line
   "RET" 'xref-goto-xref))

(use-package consult-flycheck
  :after consult)
(provide 'completion)
;;; completion.el ends here
