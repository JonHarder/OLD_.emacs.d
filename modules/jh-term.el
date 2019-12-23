;;; core --- Summary
;; Configure external terminal emulators to play nice with emacs.

;;; Commentary:
;;; As great as eshell is, there comes a time in every emacser life
;;; where they find themselves reaching for an external shell.
;;; This module aims to make the transition as painless as possible.

;;; Code:
(defun modules/term--load (config)
  "Load term stuff using CONFIG."
  (straight-use-package 'fish-mode)

  (straight-use-package 'vterm)

  (defadvice term-handle-exit
      (after term-kill-buffer-on-exit activate)
    (kill-buffer)))


(provide 'jh-term)
;;; jh-term.el ends here
