(defun modules/mail--load (config)
  "Load some general ansible packages and ignore CONFIG."
  (use-package notmuch)
  (require 'smtpmail)

  (setq auth-source-debug 'trivia)

  ;; setup address and name
  (setq mail-user-agent 'message-user-agent
        user-mail-address "jharder@kipsu.com"
        user-full-name "Jon Harder")

  (setq smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-local-domain "gmail.com")

  ;; smtp config
  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-stream-type 'ssl
        smtpmail-smtp-service 465
        message-send-mail-function 'smtpmail-send-it)

  ;; report problems with the smtp server
  (setq smtpmail-debug-info t)
  ;; add Cc and Bcc headers to the message buffer
  (setq message-default-mail-headers "Cc: \nBcc: \n")
  ;; postponed message is put in the following draft directory
  (setq message-auto-save-directory "~/mail/drafts")
  (setq message-kill-buffer-on-exit t)
  (setq message-directory "~/mail/")
       
  (add-hook 'message-mode-hook 'turn-on-auto-fill)
  (add-hook 'message-mode-hook 'mail-abbrevs-setup))
