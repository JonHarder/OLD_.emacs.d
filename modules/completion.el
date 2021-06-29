;;; completion --- Configures completion framework, currently, corfu, vertico

;;; Commentary:


;;; Code:
(require 'use-package)
(require 'evil)

(defun modules/completion--load (config)
  "Set up completion, respecting any CONFIG provided."

  (use-package corfu
    :init
    (corfu-global-mode 1)
    :custom
    (corfu-cycle t)
    (corfu-auto t)
    (corfu-quit-at-boundary t)
    (corfu-quit-no-match t))

  ;; (straight-use-package
  ;;  '(corfu :type git :host github :repo "minad/corfu"))
  ;; (require 'corfu)
  ;; (setq corfu-cycle t
  ;;       corfu-auto t
  ;;       corfu-quit-at-boundary t
  ;;       corfu-quit-no-match t)
  ;; (corfu-global-mode 1)

  (use-package vertico
    :init
    (vertico-mode)
    :custom
    (vertico-cycle t))
  
  ;; (straight-use-package
  ;;  '(vertico :type git :host github :repo "minad/vertico"))
  ;; (require 'vertico)
  ;; (vertico-mode 1)
  ;; (setq vertico-cycle t)
  ;; (define-key vertico-map (kbd "C-n") #'vertico-next)
  ;; (define-key vertico-map (kbd "C-p") #'vertico-previous)

  (setq resize-mini-windows t)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


  (use-package orderless
    :custom
    (completion-styles '(orderless))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles . (partial-completion))))))

  (use-package savehist
    :init
    (savehist-mode))
    

  (setq tab-always-indent 'complete)

  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)

  (use-package projectile
    ;; :init
    ;; (setq-default projectile-completion-system 'selectrum--read)
    :config
    (projectile-mode +1))

  (use-package consult
    :bind (("C-c h" . consult-history)
           ("C-c o" . consult-outline)
           ("C-x b" . consult-buffer)
           :map flycheck-command-map
           ("!" . consult-flycheck))
    :init
    (fset 'multi-occur #'consult-multi-occur))


  ;; (use-package selectrum
  ;;   :config
  ;;   (selectrum-mode +1))

  ;; (use-package selectrum-prescient
  ;;   :config
  ;;   (selectrum-prescient-mode +1)
  ;;   (prescient-persist-mode +1)
  ;;   :custom
  ;;   (selectrum-prescient-enable-filtering nil))

  (use-package marginalia
    :init
    (marginalia-mode)
    (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

  (use-package consult-flycheck))
(provide 'completion)
;;; completion.el ends here
