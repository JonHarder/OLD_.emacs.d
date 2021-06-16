;;; org --- Configuration for magical org mode
;;; Commentary:
;;; Here lies my configuration for org mode, including
;;; the addition of other packages which make bullets prettier, and
;;; introduce additional languages that org src blocks can evaluate.

;;; Code:
(defun modules/org--load (config)
  "Load configuration related to org using CONFIG."

  ;;; additional org libraries
  (require 'org-agenda)
  (require 'org-habit)
  (require 'org-tempo)
  (require 'ob-python)
  (require 'ob-php (concat user-emacs-directory "ob-php.el"))

  ;;; SETTINGS
  (setq org-fontify-whole-heading-line t
        org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0
        org-agenda-files '("~/Org/calendars" "~/Org/todo.org")
        org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done)
        calendar-date-style 'iso
        ;; calendar-mark-diary-entries-flag nil
        calendar-mode-line-format nil
        calendar-date-display-form calendar-iso-date-display-form
        org-todo-keywords '((sequence
                             "SOMEDAY(s)"
                             "TODO(t)"
                             "NEXT(n)"
                             "WAITING(w)"
                             "INPROGRESS(i)"
                             "REVIEW(r)"
                             "|"
                             "DONE(d)"
                             "CANCELLED(c)"))
        org-todo-keyword-faces '(("SOMEDAY" :foreground "#eeeeee" :background "#444444" :weight bold :underline t)
                                 ("TODO" :foreground "#fc9d03" :weight bold :underline t)
                                 ("NEXT" :foreground "#0098dd" :weight bold :underline t)
                                 ("WAITING" :foreground "#bf3cc6" :weight bold :underline t)
                                 ("INPROGRESS" :foreground "#fc5603" :weight bold :underline t)
                                 ("REVIEW" :foreground "#964B00" :weight bold :underline t)
                                 ("DONE" :foreground "#50a14f" :weight bold :underline t)
                                 ("CANCELLED" :foreground "#fc0303" :weight bold :underline t))
        org-log-done 'time)

  (setq-default
   org-src-fontify-natively t
   org-hide-emphasis-markers t
   org-archive-location "~/Org/archive/%s.archive::"
   ;;; FIXME: setting this to `t' breaks the agenda view for some reason
   org-agenda-include-diary nil
   org-agenda-timegrid-use-ampm nil
   org-agenda-span 'week
   org-babel-python-command "python3"
   org-highest-priority ?A
   org-lowest-priority ?F
   org-treat-insert-todo-heading-as-stage-change t
   org-log-into-drawer t
   org-capture-templates
   '(("p" "Pull Request" entry (file+headline "~/Org/pull-requests.org" "Pull Requests")
      "* TODO %?\n  SCHEDULED: %t\n  - %^L")))

  (use-package org-journal
    :custom
    (org-journal-dir "~/Org/journal/")
    (org-journal-file-type 'daily))

  ;;; Addition functionality/functions
  (defun color-org-header (tag col &optional bg-col)
     "Color the associated TAG with the color COL."
     (interactive)
     (goto-char (point-min))
     (while (re-search-forward tag nil t)
       (add-text-properties (match-beginning 0) (point-at-eol)
                            `(face (:foreground ,col ,@(if bg-col `(:background ,bg-col) nil))))))

  (defun color-org-agenda ()
    (save-excursion
      (color-org-header "Work:" "#0099cc")
      (color-org-header "Bread:" "#66CD00")
      (color-org-header "Events:" "#faebd7" "#8B4513")))
  
  (add-hook 'org-agenda-finalize-hook #'color-org-agenda)

  ;;; Install external packages
  (use-package org-tree-slide)

  (use-package ob-restclient
    :defer 5)

  (use-package org-edna
    :config
    (org-edna-mode))

  (use-package org-superstar
      :demand t
      :custom
      (org-hide-leading-stars t)
      (org-superstar-special-todo-items t)
      :hook (org-mode . org-superstar-mode))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (plantuml . t)
     (sql . t)
     (calc . t)
     (restclient . t)
     (haskell t)
     (python . t)
     (php . t)
     (js . t))))

