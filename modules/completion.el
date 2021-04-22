(defun swiper--nohighlight (orig-func &rest args)
  "Get rid of the highlighting after exiting ORIG-FUNC called with ARGS."
  (apply orig-func args)
  (evil-ex-nohighlight))


(defun modules/completion--load (config)
  "Set up completion, respecting any CONFIG provided."
  (use-package company
    :config
    (global-company-mode t)
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.3))
  (use-package company-box
    :hook (company-mode . company-box-mode))

  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)

  (use-package projectile
    :defer 3
    :init
    (setq-default projectile-completion-system 'selectrum--read)
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


  (use-package selectrum)
  (use-package selectrum-prescient
    :config
    (selectrum-mode +1)
    (selectrum-prescient-mode +1)
    (prescient-persist-mode +1))

  (use-package orderless
    :custom (completion-styles '(orderless))
    :config
    (setq selectrum-refine-candidates-function #'orderless-filter
          selectrum-highlight-candidates-function #'orderless-highlight-matches))


  (use-package consult-flycheck)

  (use-package marginalia
    :init
    (marginalia-mode)
    (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))))
