;;; -*- lexical-binding: t -*-
(use-package haskell-mode
  :mode "\\.hs\\'"
  :config
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (custom-set-variables
     '(haskell-process-suggest-remove-import-lines t)
     '(haskell-process-auto-import-loaded-modules t)
     '(haskell-process-log t)))
(provide 'jh-haskell)
;;; jh-haskell.el ends here
