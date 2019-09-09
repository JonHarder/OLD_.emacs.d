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
    :config
    (company-mode t))

  (use-package projectile
    :init
    (setq-default projectile-completion-system 'ivy)
    :config
    (projectile-mode +1))
  
  (use-package counsel)
  
  (use-package amx
    :config
    (amx-mode))
  
  (use-package ivy
    :after (counsel general evil)
    :config
    (progn
      (setq-default
       ivy-use-virtual-buffers 1
       enable-recursive-minibuffers t
       ivy-height 20)
      (ivy-mode 1)))
  
  
  (use-package ivy-hydra)
  
  
  (use-package ivy-rich
    :after ivy
    :config
    (progn
      (setq-default
       ivy-virtual-abbreviate 'full
       ivy-rich-path-style 'abbrev
       ivy-format-function #'ivy-format-function-line)
      (ivy-rich-mode)))

  (advice-add 'swiper :around #'swiper--nohighlight))




(provide 'jh-completion)
;;; jh-completion.el ends here
