;;; mail --- Settings to configure email in emacs

;;; Commentary:
;;; this configuration makes use of mbsync for mail sychronization
;;; and notmuch for viewing and writing mail.

;;; Code:
(require 'message)
(require 'smtpmail)
(require 'use-package)

(use-package notmuch
  :commands notmuch
  :custom
  (notmuch-saved-searches
   '((:name "today"    :query "date:today and tag:inbox" :key "t")
     (:name "inbox"    :query "tag:inbox" :key "i")
     (:name "unread"   :query "tag:unread" :key "u")
     (:name "flagged"  :query "tag:flagged" :key "f")
     (:name "sent"     :query "tag:sent" :key "t")
     (:name "drafts"   :query "tag:draft" :key "d")
     (:name "all mail" :query "*")
     (:name "archived" :query "tag:archived" :key "a")
     (:name "todo"     :query "tag:todo" :key "o")
     (:name "security" :query "tag:security" :key "s")))
  (notmuch-archive-tags '("-inbox" "-unread"))
  :general
  ;;; Global map to enter notmuch
  (:states 'normal
   :prefix "SPC"
   "a m" 'notmuch)
  ;;; bindings for viewing emails
  (:states 'normal
   :keymaps 'notmuch-show-mode-map
   "q" #'notmuch-bury-or-kill-this-buffer)
  ;;; bindings for the emails list view
  (:states 'normal
   :keymaps 'notmuch-search-mode-map
   "j" #'notmuch-search-next-thread
   "k" #'notmuch-search-previous-thread
   "RET" #'notmuch-search-show-thread
   "m" #'notmuch-mua-new-mail
   "g r" #'notmuch-refresh-this-buffer
   "-" #'notmuch-search-remove-tag
   "+" #'notmuch-search-add-tag
   "/" #'notmuch-search
   "a" #'notmuch-search-archive-thread
   "q" #'notmuch-bury-or-kill-this-buffer
   "t" #'notmuch-tree-from-search-current-query
   "T" #'notmuch-unthreaded-from-search-current-query)
  (:states 'normal
   :keymaps 'notmuch-tree-mode-map
   "q" #'notmuch-bury-or-kill-this-buffer
   "j" #'notmuch-tree-next-matching-message
   "k" #'notmuch-tree-prev-matching-message
   "t" #'notmuch-unthreaded-from-tree-current-query))

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
