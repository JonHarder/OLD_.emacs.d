;;; irc --- Summary
;;; Configuration for the built in erc emacs package.

;;; Commentary:

;;; Code:
(require 'erc)

(defun modules/erc--load (config)
  "Load configuration related to erc using CONFIG."
  ;; (load "~/.emacs.d/.erc-auth")
  (setq erc-server "irc.libera.chat"
        erc-nick "dazed_and_amused"))

(provide 'irc)
;;; irc.el ends here
