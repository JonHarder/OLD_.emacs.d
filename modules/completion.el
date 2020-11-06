(defun swiper--nohighlight (orig-func &rest args)
  "Get rid of the highlighting after exiting swiper."
  (apply orig-func args)
  (evil-ex-nohighlight))


(defun modules/completion--load (config)
  (use-package company
    :config
    (global-company-mode t)
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.0))

  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)

  (use-package projectile
    :defer 3
    :init
    (setq-default projectile-completion-system 'selectrum-read)
    :config
    (projectile-mode +1))

  (use-package selectrum)
  (use-package selectrum-prescient
    :config
    (selectrum-mode +1)
    (selectrum-prescient-mode +1)
    (prescient-persist-mode +1)))
