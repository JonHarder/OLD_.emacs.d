(use-package racket-mode
  :commands (racket-repl)
  :mode "\\.rkt\\'"
  :custom
  (racket-program "/Applications/Racket_v8.2/bin/racket")
  :hook (racket-mode . racket-xp-mode))
