;;; mail --- Settings to configure email in emacs

;;; Commentary:
;;; this configuration makes use of mbsync for mail sychronization
;;; and notmuch for viewing and writing mail.

;;; Code:
(use-package notmuch
  :commands notmuch
  :custom
  (notmuch-saved-searches
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
     (:name "smeagles" :query "tag:smeagle" :key "m")))
  :config
  (defun check-mail ()
    "Execute the system check-mail command, then open email client."
    (interactive)
    (shell-command "check-mail")
    (notmuch))
  :general
  (:states 'normal
   :prefix "SPC"
   "a M" 'notmuch
   "a m" 'check-mail))

(use-package auth-source
  :custom
  (auth-source-debug 'trivia))

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
(provide 'jh-mail)
;;; jh-mail.el ends here
