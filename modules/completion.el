;;; completion --- Configures completion framework, currently, corfu, vertico -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(use-package corfu
  :init
  (corfu-global-mode 1)
  :config
  (setq corfu-cycle t)
  (setq corfu-preselect-first nil)
  (setq corfu-auto t)
  (setq corfu-quit-at-boundary t)
  (setq corfu-quit-no-match t)
  (setq corfu-auto-delay 0.6)
  :general
  (:keymaps 'corfu-map
   :states '(normal insert)
   "TAB" 'corfu-next
   [tab] 'corfu-next
   "S-TAB" 'corfu-previous
   [backtab] 'corfu-previous))

;; (with-eval-after-load 'corfu
;;   (straight-use-package
;;   '(corfu-doc :type git :host github :repo "galeo/corfu-doc"))
;;  (require 'corfu-doc)
;;  (add-hook 'corfu-mode-hook #'corfu-doc-mode))
   


;; (use-package cape
;;   :init
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-keyword)
;;   (add-to-list 'completion-at-point-functions #'cape-symbol))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
             

(use-package emacs
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  (completion-cycle-threshold 3)
  (completions-detailed t)
  (tab-always-indent 'complete)
  (resize-mini-windows nil)
  (resize-mini-frames nil)
  (enable-recursive-minibuffers t)
  :config
  (minibuffer-depth-indicate-mode 1))

(use-package vertico
  :hook (emacs-startup . vertico-mode)
  :custom
  (vertico-cycle t)
  :config
  (add-to-list 'load-path "~/.emacs.d/straight/repos/vertico/extensions")
  (require 'vertico-directory)
  (require 'vertico-reverse)
  (require 'vertico-quick)
  ;; (vertico-reverse-mode 1)
  (general-define-key
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
  (completion-styles '(substring initials flex partial-completion orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (basic partial-completion orderless))))))

(use-package embark
  ;; :after (vertico marginalia)
  :commands (embark-dwim embark-act embark-prefix-help-command)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (embark-define-keymap embark-tab-actions
    "Keymap for actions for tab-bar tabs (when mentioned by name)."
    ("r" tab-bar-rename-tab-by-name)
    ("d" tab-bar-close-tab-by-name))
  (add-to-list 'embark-keymap-alist '(tab . embark-tab-actions))
  ;; hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :general
  ("C-;" 'embark-act
   "M-;" 'embark-dwim))

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
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  :custom
  (consult-narrow-key "<")
  :general
  (:keymaps 'override
   "C-c h" #'consult-history
   "C-x b" #'consult-buffer)
  (:keymaps 'eshell-mode-map
   :states 'insert
   "M-r" #'consult-history)
  (:keymaps 'flycheck-command-map
   :states 'normal
   "!" #'consult-flycheck)
  (:prefix ","
   :states 'normal
   "o" 'consult-outline)
  (:prefix "SPC"
   :states 'normal
   "b b" 'consult-buffer
   "b m" 'consult-bookmark
   "i i" 'consult-imenu
   "h a" 'consult-apropos
   "l l" 'consult-flycheck
   "o o" #'consult-outline
   "/" 'consult-ripgrep))

(use-package embark-consult
  :after (consult embark)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
  :init
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))
  :config
  (add-to-list 'marginalia-prompt-categories '("tab by name" . tab))
  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "SPC") nil)
    (define-key map (kbd "?") nil)))

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
