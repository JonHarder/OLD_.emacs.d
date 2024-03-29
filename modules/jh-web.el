(use-package emmet-mode
  :hook 'sgml-mode)
  
(use-package web-mode
  :mode "\\.vue\\'")

(use-package javascript-mode)

(use-package prettier-js
  :disabled t
  :mode "\\.js\\'"
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode))
    
(use-package web-mode
  :mode ("\\.html\\'" "\\.html.erb\\'"))
(provide 'jh-web)
;;; jh-web.el ends here
