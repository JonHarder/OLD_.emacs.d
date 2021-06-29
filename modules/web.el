(defun modules/web--load (config)
  "Load configuration for web based modes using CONFIG."
  (use-package emmet-mode
    :hook 'sgml-mode)
  
  (use-package web-mode
    :mode "\\.vue\\'")

  (use-package prettier-js
    :config
    (add-hook 'js2-mode-hook 'prettier-js-mode)
    (add-hook 'web-mode-hook 'prettier-js-mode))
    

  (use-package rjsx-mode)
  
  (use-package typescript-mode
    :mode "\\.tsx\\'")

  (use-package web-mode
    :mode "\\.html\\'"))
