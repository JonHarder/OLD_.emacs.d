;;; mail --- Summary


;;; Commentary:

;;; Code:
(defun modules/mail--load (config)
  "Load some general ansible packages and ignore CONFIG."
  (use-package notmuch
    :demand t)
  (require 'smtpmail)
  (setq mail-user-agent 'message-user-agent
        user-mail-address "jharder@kipsu.com"
        user-full-name "Jon Harder"
        smtpmail-smtp-server "smtp.gmail.com"
        message-send-mail-function 'message-smtpmail-send-it
        message-kill-buffer-on-exit t))


(provide 'jh-mail)
;;; jh-mail.el ends here
