;;; work --- Summary
;; tweaks and shortcuts for tasks at work

;;; Commentary:

;;; Code:

(defun jh/jira-link (jira-number)
  (interactive "nTicket Number:")
  (insert (format "https://kipsudev.atlassian.net/browse/KIPSU-%i" jira-number)))

(use-package restclient
  :straight t)

(provide 'jh-work)
;;; jh-work.el ends here
