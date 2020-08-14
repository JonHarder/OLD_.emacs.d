;;; haskell --- Summary


;;; Commentary:

;;; Code:
(defun modules/haskell--load (config)
  "Load configuration for Haskell using CONFIG."
  (straight-use-package 'haskell-mode)
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (custom-set-variables
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t))

  (defun jh/haskell-stack-run ()
    "Compile and run a haskell application using 'stack run'."
    (interactive)
    (compile "stack run"))

  (use-package lsp-haskell
    :config
    (setq lsp-haskell-process-path-hie "/Users/jharder/.local/bin/hie-wrapper")
    (add-hook 'haskell-mode-hook #'lsp)))

  ;; (use-package lsp-haskell
  ;;   :demand
  ;;   :config
  ;;   (progn
  ;;     (setq lsp-haskell-process-path-hie "/home/jon/.local/bin/hie-wrapper")
  ;;     (add-hook 'haskell-mode-hook #'lsp))))

(provide 'jh-haskell)
;;; jh-haskell.el ends here
