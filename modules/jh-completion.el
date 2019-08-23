;;; completion --- Summary
;; configure completion engine : Ivy

;;; Commentary:

;;; Code:

(defun swiper--nohighlight (orig-func &rest args)
  "Get rid of the highlighting after exiting swiper."
  (apply orig-func args)
  (evil-ex-nohighlight))


(defun modules/completion--load (config)
  (use-package company
    :straight t
    :config
    (company-mode t))

  (use-package projectile
    :straight t
    :init
    (setq projectile-completion-system 'ivy)
    :config
    (projectile-mode +1))
  
  (use-package counsel
    :straight t)
  
  (use-package amx
    :straight t
    :config
    (amx-mode))
  
  (use-package ivy
    :after (counsel general evil)
    :straight t
    :init
    (setq ivy-use-virtual-buffers 1
          enable-recursive-minibuffers t
          ivy-height 20)
    :config
    (ivy-mode 1))
  
  
  (use-package ivy-hydra
    :straight t)
  
  
  (use-package ivy-rich
    :after ivy
    :straight t
    :init
    (setq ivy-virtual-abbreviate 'full
          ivy-rich-path-style 'abbrev
          ivy-format-function #'ivy-format-function-line)
    :config
    (ivy-rich-mode))

  (advice-add 'swiper :around #'swiper--nohighlight))




(provide 'jh-completion)
;;; jh-completion.el ends here
