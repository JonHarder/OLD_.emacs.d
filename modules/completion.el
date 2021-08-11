;;; completion --- Configures completion framework, currently, corfu, vertico -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:
(use-package corfu
  :ensure t
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
  (minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (enable-recursive-minibuffers t)
  :config
  (minibuffer-depth-indicate-mode 1)
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package vertico
  :ensure t
  :demand t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  :config
  (require 'vertico-directory) ;; ~/.emacs.d/ext_lisp/vertico-directory.el
  :general
  (:keymaps 'vertico-map
   :states '(insert normal)
   "C-n" 'vertico-next
   "C-p" 'vertico-previous
   "DEL" 'vertico-directory-delete-char
   "M-DEL" 'vertico-directory-delete-word
   "RET" 'vertico-directory-enter))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(substring initials flex partial-completion orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package embark
  :ensure t
  :after (vertico marginalia)
  :commands (embark-dwim embark-act embark-prefix-help-command)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :general
  (:keymaps 'vertico-map
   :states '(insert normal)
   "C-." 'embark-dwim
   "C-;" 'embark-act))

(use-package recentf
  :hook pre-command
  :config
  (recentf-mode 1))

(use-package savehist
  :ensure t
  :hook pre-command
  :init
  (savehist-mode))

(use-package consult
  :ensure t
  :init
  (fset 'multi-occur #'consult-multi-occur)
  :general
  ("C-c h" #'consult-history
   "C-c o" #'consult-outline
   "C-x b" #'consult-buffer)
  (:keymaps 'flycheck-command-map
   :states 'normal
   "!" #'consult-flycheck)
  (:prefix "SPC"
   :states 'normal
   "b b" 'consult-buffer
   "i i" 'consult-imenu
   "h a" 'consult-apropos
   "l l" 'consult-flycheck))

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

(use-package consult-flycheck
  :ensure t
  :after consult)
(provide 'completion)
;;; completion.el ends here
