(defun modules/web--load (config)
  "Load configuration for web based modes using CONFIG."
  (use-package emmet-mode
    :ensure t
    :hook 'sgml-mode)
  
  (use-package web-mode
    :ensure t
    :mode "\\.vue\\'")

  (use-package prettier-js
    :ensure t
    :mode "\\.js\\'"
    :config
    (add-hook 'js2-mode-hook 'prettier-js-mode)
    (add-hook 'web-mode-hook 'prettier-js-mode))
    
  (use-package web-mode
    :ensure t
    :mode "\\.html\\'"))
