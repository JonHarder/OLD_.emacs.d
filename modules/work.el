(defvar jh/work-base-dir "~/Kipsu/ansible-playbooks/vagrant_kipsu/")
(defvar jh/work-dir (format "%sacct/" jh/work-base-dir))

(defun jh/jira-link (jira-number)
  "Generate the jira link given a JIRA-NUMBER."
  (interactive "nTicket Number:")
  (insert (format "[[https://kipsudev.atlassian.net/browse/KIPSU-%i][KIPSU-%i]]"
                  jira-number
                  jira-number)))

(defun vagrant-up ()
  (interactive)
  (async-shell-command (format "cd %s && vagrant up" jh/work-base-dir)))

(defun vagrant-halt ()
  (interactive)
  (async-shell-command (format "cd %s && vagrant halt" jh/work-base-dir)))
  
(use-package restclient
  :commands restclient-mode
  :mode ("\\.http\\'" . restclient-mode)
  :general
  (:keymaps 'restclient-mode-map
   :states 'normal
   "q" 'quit-window))

(use-package zpresent
  :commands zpresent)

(use-package coffee-mode
  :mode "\\.coffee\\'")

(defun jh/work-git ()
  (interactive)
  (magit-status jh/work-dir))

(defun jh/work-find-file ()
  (interactive)
  (let ((default-directory jh/work-dir))
    (call-interactively #'find-file)))

(defun jh/standup-today ()
  "Open or create an agenda for a standup."
  (interactive)
  (let ((datestr (format-time-string "%Y%m%d")))
    (find-file (format "~/Org/standups/%s.org" datestr))))

(require 'request)

(defun quore-request (token action)
  "Use the provider TOKEN, make a request to the ACTION enpdoint of Quore."
  (let ((oauth-header (concat "OAuth oauth_token=" token))
        (response nil))
    (request "https://api.quore.com/api.php"
      :sync t
      :type "POST"
      :params `(("action" . ,action))
      :parser #'json-read
      :headers `(("Accept" . "application/json")
                 ("Version" . "2")
                 ("Authorization" . ,oauth-header))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq response data))))
    response))

(defun get-quore-token-id (token)
  "Get the quore token id from the TOKEN."
  (let ((json (quore-request token "getTokenInfo")))
    (cdr (assoc 'id (elt json 0)))))

(provide 'work)
;;; work.el ends here
