;;; irc --- Summary
;;; Configuration for the built in erc emacs package.

;;; Commentary:

;;; Code:
(require 'erc)

;; (load "~/.emacs.d/.erc-auth")
(use-package erc
  :straight nil
  :custom
  (erc-server "irc.libera.chat")
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-prompt-for-password nil))

(provide 'jh-irc)
;;; jh-irc.el ends here
