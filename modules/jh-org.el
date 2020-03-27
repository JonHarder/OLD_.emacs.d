;;; org --- Summary
;; All things related or org mode configuration.

;;; Commentary:

;;; Code:
(require 'org)
(require 'org-agenda)
(require 'ob-python)
(require 'ob-php (concat user-emacs-directory "ob-php.el"))


(defun modules/org--load (config)
  "Load configuration related to org using CONFIG."
  (setq-default
   org-agenda-files '("~/Org" "~/Org/agenda/")
   org-src-fontify-natively t
   org-hide-emphasis-markers nil
   org-archive-location "~/Org/archive/%s.archive::"
   org-agenda-include-diary t
   org-agenda-timegrid-use-ampm t
   org-babel-python-command "python3"
   org-highest-priority ?A
   org-lowest-priority ?F
   org-capture-templates
   '(("p" "Pull Request" entry (file+headline "~/Org/pull-requests.org" "Pull Requests")
      "* TODO %?\n  SCHEDULED: %t\n  - %^L")))

  (use-package ob-mermaid)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (sql . t)
     (mermaid . t)
     (haskell t)
     (python . t)
     (php . t)
     (js . t)))

  
  (use-package org-superstar
    :custom
    (org-hide-leading-stars t)
    (org-superstar-special-todo-items t)
    :hook (org-mode . org-superstar-mode))
  
  (setq org-todo-keywords
        '((sequence
           "BLOCKED(b)"
           "TODO(t)"
           "WORKING(w)"
           "REVIEW(r)"
           "TESTING(s)"
           "|"
           "DONE(d)"
           "DEFERRED(f)"
           "CANCELED(c@)"))))

(provide 'jh-org)
;;; jh-org.el ends here
