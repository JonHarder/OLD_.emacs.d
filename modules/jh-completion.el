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
    (setq-default projectile-completion-system 'selectrum-read)
    :config
    (projectile-mode +1))
  
  ;;; (use-package counsel)
  
  ;; (use-package amx
  ;;   :config
  ;;   (amx-mode))

  (straight-use-package 'selectrum-prescient)
  (selectrum-mode +1)
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))
  
  ;; (use-package ivy
  ;;   :after (counsel general evil)
  ;;   :custom
  ;;   (ivy-use-virtual-buffers 1)
  ;;   (ivy-hight 30)
  ;;   (ivy-virtual-abbreviate 'full)
  ;;   (ivy-format-function #'ivy-format-function-line)
  ;;   :config
  ;;   (setq-default enable-recursive-minibuffers t)
  ;;   (ivy-mode 1))
  
  
  ;; (use-package ivy-prescient
  ;;   :config
  ;;   (ivy-prescient-mode))
  ;; (straight-use-package 'ivy-hydra)
  
  
  ;; (use-package ivy-rich
  ;;   :after ivy
  ;;   :custom
  ;;   (ivy-rich-path-style 'abbrev)
  ;;   :config
  ;;   (ivy-rich-mode))

  ;; (advice-add 'swiper :around #'swiper--nohighlight))




(provide 'jh-completion)
;;; jh-completion.el ends here
