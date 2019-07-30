;;; org --- Summary
;; All things related or org mode configuration.

;;; Commentary:

;;; Code:

(setq org-agenda-files '("~/Org")
      org-src-fontify-natively t
      org-archive-location (concat user-emacs-directory
                                   "archive/%s.archive::")
      org-agenda-timegrid-use-ampm t)


(setq org-babel-python-command "python3")
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (shell . t)
    (python . t)))


(use-package org-journal
  :straight t
  :init
  (setq org-journal-dir "~/Org/journal/"))

(use-package org-bullets
  :straight t
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
           "CANCELED(c@)")))

(provide 'jh-org)
;;; jh-org.el ends here
