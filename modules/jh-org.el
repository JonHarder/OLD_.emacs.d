;;; org --- Summary
;; All things related or org mode configuration.

;;; Commentary:

;;; Code:
(require 'org)
(require 'org-agenda)
(require 'ob-python)

(defun modules/org--load (config)
  "Load configuration related to org using CONFIG."
  (setq-default
   org-agenda-files '("~/Org" "~/Org/agenda/")
   org-src-fontify-natively t
   org-hide-emphasis-markers nil
   org-archive-location "~/Org/archive/%s.archive::"
   org-agenda-include-diary t
   org-agenda-timegrid-use-ampm t
   org-babel-python-command "python3")
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)))
  
  (use-package org-bullets
    :config
    (add-hook 'org-mode-hook
              (lambda ()
		(org-bullets-mode 1))))
  
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "WAIT(w@/!)"
           "|"
           "DONE(d!)"
           "CANCELED(c@)"))))

(provide 'jh-org)
;;; jh-org.el ends here
