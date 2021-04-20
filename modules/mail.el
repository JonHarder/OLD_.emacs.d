(defun modules/mail--load (config)
  "Load some general ansible packages and ignore CONFIG."
  (use-package notmuch
    :config
    (setq notmuch-saved-searches
          '((:name "inbox"    :query "tag:inbox" :key "i")
            (:name "unread"   :query "tag:unread" :key "u")
            (:name "flagged"  :query "tag:flagged" :key "f")
            (:name "sent"     :query "tag:sent" :key "t")
            (:name "drafts"   :query "tag:draft" :key "d")
            (:name "all mail" :query "*")
            (:name "today"    :query "date:today" :key "t")
            (:name "archived" :query "tag:archived" :key "a")
            (:name "china"    :query "tag:china" :key "c")
            (:name "todo"     :query "tag:todo" :key "o")
            (:name "security" :query "tag:security" :key "s")
            (:name "smeagles" :query "tag:smeagle" :key "m"))))
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
  (add-hook 'message-mode-hook 'mail-abbrevs-setup)

  (defun check-mail ()
    (interactive)
    (shell-command "check-mail")
    (notmuch)))
