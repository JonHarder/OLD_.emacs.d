(use-package racket-mode
  :commands (racket-repl)
  :mode "\\.rkt\\'"
  :custom
  (racket-program "/Applications/Racket_v8.2/bin/racket")
  :hook (racket-mode . racket-xp-mode)
  :general
  (:keymaps 'racket-mode-map
   :states 'normal
   :prefix ","
   "RET" #'racket-run-module-at-point))

(provide 'jh-racket)
;;; jh-racket ends here
