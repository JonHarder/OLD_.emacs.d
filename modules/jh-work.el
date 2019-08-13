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

(provide 'jh-work)
;;; jh-work.el ends here
