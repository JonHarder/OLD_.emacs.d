;;; completion --- Configures completion framework, currently, corfu, vertico -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
;; (require 'svg-lib)

(use-package corfu
  :demand t
  :after general
  :hook (lsp-completion-mode . jh/corfu-setup-lsp)
  :init
  (corfu-global-mode 1)
  :custom
  (corfu-echo-documentation nil)
  (corfu-cycle t)
  (corfu-preselect-first t)
  (corfu-auto t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-auto-delay 0.6)
  (lsp-completion-provider :none)
  :config
  (defun jh/corfu-setup-lsp ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :general
  (:keymaps 'corfu-map
   :states '(normal insert)
   "TAB" 'corfu-next
   [tab] 'corfu-next
   "S-TAB" 'corfu-previous
   [backtab] 'corfu-previous))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu-doc
  :straight (corfu-doc :type git :host github :repo "galeo/corfu-doc")
  :after corfu
  :general (:keymaps 'corfu-map
                     [remap corfu-show-documentation] #'corfu-doc-toggle
                     "M-n" #'corfu-doc-scroll-up
                     "M-p" #'corfu-doc-scroll-down)
  :custom
  (corfu-doc-delay 0.5)
  (corfu-doc-max-width 70)
  (corfu-doc-max-height 20)
  (corfu-echo-documentation nil)
  :config
  (corfu-doc-mode))

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
  :after general
  :hook (emacs-startup . vertico-mode)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive))
  :custom
  (vertico-cycle t)
  (vertico-count 13)
  (vertico-resize nil)
  :config
  (add-to-list 'load-path "~/.emacs.d/straight/repos/vertico/extensions")
  (require 'vertico-directory)
  (require 'vertico-reverse)
  (require 'vertico-quick)
  (general-define-key
   :keymaps 'vertico-map
   "C-n"   'vertico-next
   "C-p"   'vertico-previous
   "<escape>" #'keyboard-escape-quit)
  (defvar vertico-cand-once nil)
  (unless vertico-cand-once
   (advice-add #'vertico--format-candidate :around
               (lambda (orig cand prefix suffix index _start)
                 (setq vertico-cand-once t)
                 (setq cand (funcall orig cand prefix suffix index _start))
                 (concat
                  (if (= vertico--index index)
                      (propertize "» " 'face 'vertico-current)
                    "  ")
                  cand)))))

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
  (completion-category-overrides '((file (styles . (basic partial-completion orderless))))))

(use-package embark
  ;; :after (vertico marginalia)
  :after general
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
  :after general
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
   "o o" #'consult-outline))
   ;; "/" 'consult-ripgrep))

(use-package embark-consult
  :after (consult embark)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-dir
  :after (evil general)
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
  :after general
  :straight nil
  :general
  (:keymaps 'xref--xref-buffer-mode-map
   :states 'normal
   "j" 'xref-next-line
   "k" 'xref-prev-line
   "RET" 'xref-goto-xref))

(use-package consult-flycheck
  :after consult)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package link-hint
  :general
  (:states 'normal
   :prefix "SPC"
   "l o" #'link-hint-open-link))

(provide 'jh-completion)
;;; jh-completion.el ends here
