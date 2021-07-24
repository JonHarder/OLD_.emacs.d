(defun modules/racket--load (config)
  (use-package racket-mode
    :ensure t
    :commands (racket-repl)
    :mode "\\.rkt\\'"))
