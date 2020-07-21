;;; erc --- Summary
;; All things related to erc irc chat configuration.

;;; Commentary:

;;; Code:

(defun modules/erc--load (config)
  "Load configuration related to erc using CONFIG."
  (load "~/.emacs.d/.erc-auth")

  (defun jh/erc ()
    (interactive)
    (erc :server "irc.freenode.net"
         :port "6667"
         :nick jh/erc-nick
         :password jh/erc-passwd)))

                                 

(provide 'jh-erc)
;;; jh-erc.el ends here
