(defun modules/web--load (config)
  "Load configuration for web based modes using CONFIG."
  (use-package emmet-mode
    :hook 'sgml-mode)
  
  (use-package web-mode
    :mode "\\.vue\\'")

  (straight-use-package 'rjsx-mode)
  
  (use-package typescript-mode
    :mode "\\.tsx\\'")

  (use-package web-mode
    :mode "\\.html\\'"))
