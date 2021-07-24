;;; irc --- Summary
;;; Configuration for the built in erc emacs package.

;;; Commentary:

;;; Code:
(require 'erc)

(defun modules/irc--load (config)
  "Load configuration related to erc using CONFIG."
  ;; (load "~/.emacs.d/.erc-auth")
  (use-package erc
    :custom
    (erc-server "irc.libera.chat")
    (erc-hide-list '("JOIN" "PART" "QUIT"))
    (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
    (erc-prompt-for-password nil)))

(provide 'irc)
;;; irc.el ends here
