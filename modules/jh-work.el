;;; work --- Summary
;; tweaks and shortcuts for tasks at work

;;; Commentary:

;;; Code:



(defun modules/work--load (config)
  "Load various helper packages and functions according to CONFIG."


  (defvar jh/kipsu-dir "~/Kipsu/ansible-playbooks/vagrant_kipsu/acct")

  (defun jh/jira-link (jira-number)
    "Generate the jira link given a JIRA-NUMBER."
    (interactive "nTicket Number:")
    (insert (format "https://kipsudev.atlassian.net/browse/KIPSU-%i" jira-number)))
  
  (use-package restclient
    :mode ("\\.http\\'" . restclient-mode))

  (straight-use-package 'zpresent)
  (straight-use-package 'coffee-mode)
  (straight-use-package 'wsd-mode)
  (straight-use-package 'notmuch)

  (use-package mermaid-mode
    :mode ("\\.mermaid\\'" . mermaid-mode))

  (use-package ob-mermaid
    :custom
    (ob-mermaid-cli-path "~/bin/mmdc")
    (mermaid-mmdc-location "~/bin/mmdc")
    (mermaid-output-format ".svg"))

  (defun jh/kipsu-git ()
    (interactive)
    (magit-status jh/kipsu-dir))

  (defun jh/standup-today ()
    "Open or create an agenda for a standup."
    (interactive)
    (let ((datestr (format-time-string "%Y%m%d")))
      (find-file (format "~/Org/standups/%s.org" datestr)))))

(provide 'jh-work)
;;; jh-work.el ends here
