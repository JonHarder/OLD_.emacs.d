;;; core --- Summary
;; Configure external terminal emulators to play nice with emacs.

;;; Commentary:
;;; As great as eshell is, there comes a time in every emacser life
;;; where they find themselves reaching for an external shell.
;;; This module aims to make the transition as painless as possible.

;;; Code:
(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))


(use-package fish-mode
  :ensure t)


(provide 'term)
;;; term.el ends here
