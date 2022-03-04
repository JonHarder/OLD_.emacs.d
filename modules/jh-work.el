(require 'dash)

(defvar jh/work-base-dir "~/Kipsu/ansible-playbooks/vagrant_kipsu/")
(defvar jh/work-dir (format "%sacct/" jh/work-base-dir))

(defun jh/jira-link (jira-number)
  "Generate the jira link given a JIRA-NUMBER."
  (interactive "nTicket Number:")
  (insert (format "[[https://kipsudev.atlassian.net/browse/KIPSU-%i][KIPSU-%i]]"
                  jira-number
                  jira-number)))

(defvar pull-request-checklist-items
  '("Syntax Errors"
    "Adequate Logging"
    "Error handling"
    "Security considerations"))

(defun jh/generate-pull-request-checklist (title url author date)
  "Generate a string representing org mode pull request checklist with TITLE at URL by AUTHOR on DATE."
  (let ((items (string-join (mapcar (lambda (item)
                                      (format "- [ ] %s" item))
                                    pull-request-checklist-items)
                            "\n"))
        (pr-block (if (fboundp 'jh/org-code-review-block)
                      (format "\n%s\n" (or (jh/org-code-review-block url) ""))
                    "")))
    (format "#+TITLE: %s
#+AUTHOR: %s
#+DATE: <%s>
* Link
- %s%s
* Checklist
%s" title author date url pr-block items)))


(defun pull-request-checklist (title url author)
  "Generate a pull request checklist titled TITLE for URL written by AUTHOR."
  (interactive "sTitle: \nsPull request: \nsAuthor: ")
  (let* ((current-date (format-time-string "%Y-%m-%d"))
         (sanitized-title (--> title
                               (downcase it)
                               (replace-regexp-in-string "['\"?]" "" it)
                               (replace-regexp-in-string "\s" "_" it)))
         (fname (format "~/Dropbox/Work/pull-requests/%s_%s.org" current-date sanitized-title)))
    (find-file-other-window fname)
    (insert (jh/generate-pull-request-checklist title url author current-date))
    (save-buffer)))

(defun vagrant-up ()
  (interactive)
  (async-shell-command (format "cd %s && vagrant up" jh/work-base-dir)))

(defun vagrant-halt ()
  (interactive)
  (async-shell-command (format "cd %s && vagrant halt" jh/work-base-dir)))
  
(use-package restclient
  :after general
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

(defun jh/standup ()
  "Open or create an agenda for a standup."
  (interactive)
  (find-file-other-window "~/Dropbox/work/standup.org"))

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

(provide 'jh-work)
;;; jh-work.el ends here
