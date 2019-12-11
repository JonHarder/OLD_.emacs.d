;;; work --- Summary
;; tweaks and shortcuts for tasks at work

;;; Commentary:

;;; Code:

(defun jh/jira-link (jira-number)
  "Generate the jira link given a JIRA-NUMBER."
  (interactive "nTicket Number:")
  (insert (format "https://kipsudev.atlassian.net/browse/KIPSU-%i" jira-number)))

(straight-use-package 'restclient)
(straight-use-package 'zpresent)
(straight-use-package 'coffee-mode)
(straight-use-package 'notmuch)


(defun jh/standup-today ()
  "Open or create an agenda for a standup."
  (interactive)
  (let ((datestr (format-time-string "%Y%m%d")))
    (find-file (format "~/Org/standups/%s.org" datestr))))


(defun modules/work--load (config))

(provide 'jh-work)
;;; jh-work.el ends here
