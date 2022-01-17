;;; org --- Configuration for magical org mode
;;; Commentary:
;;; Here lies my configuration for org mode, including
;;; the addition of other packages which make bullets prettier, and
;;; introduce additional languages that org src blocks can evaluate.

;;; Code:
;; (require 'org-agenda)
;; (require 'org-capture)
(require 'evil)
(require 'general)

(defun org-kipsu-open (ticket _)
  "Open ticket TICKET in kipsu jira."
  (browse-url-chrome (format "https://kipsudev.atlassian.net/browse/KIPSU-%s" ticket)))

(with-eval-after-load 'evil
  (defun insert-timed-note ()
    "Insert a list item with the current hour:minute."
    (interactive)
    (evil-org-meta-return)
    (insert (format-time-string "%H:%M "))))

(with-eval-after-load 'org
  (org-link-set-parameters "kipsu"
                           :follow #'org-kipsu-open))


(defun color-org-header (tag col &optional bg-col)
   "Color the associated TAG with the color COL, using BG-COL if provided for the background."
   (interactive)
   (goto-char (point-min))
   (while (re-search-forward tag nil t)
     (add-text-properties (match-beginning 0) (point-at-eol)
                          `(face (:foreground ,col ,@(if bg-col `(:background ,bg-col) nil))))))


(defun color-org-agenda ()
  "Color specific org agenda events."
  (save-excursion
    (color-org-header "Work:" "#0099cc")
    (color-org-header "Bread:" "#66CD00")
    (color-org-header "Events:" "#faebd7" "#8B4513")))
  
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (require 'ox-md)
  (require 'org-agenda)
  (require 'org-habit)
  (require 'org-tempo)
  (require 'ob-python)
  (require 'ob-php (concat user-emacs-directory "ob-php.el"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     ;; (plantuml . t)
     (sql . t)
     (calc . t)
     (restclient . t)
     (haskell t)
     (python . t)
     (php . t)
     (C . t)
     (js . t)))
  (setq-default
   org-latex-packages-alist '(("margin=1in" "geometry" nil))
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
   org-log-into-drawer t)
  (add-to-list 'org-latex-default-packages-alist '("greek" "babel" t))

  (if jh/scale-org-headings
      (progn
        (face-spec-set 'org-level-1 `((t (:height 1.5))))
        (face-spec-set 'org-level-2 `((t (:height 1.3))))
        (face-spec-set 'org-level-3 `((t (:height 1.2))))
        (face-spec-set 'org-level-4 `((t (:height 1.1)))))
    (progn
      (face-spec-set 'org-level-1 `((t (:height unspecified))))
      (face-spec-set 'org-level-2 `((t (:height unspecified))))
      (face-spec-set 'org-level-3 `((t (:height unspecified))))
      (face-spec-set 'org-level-4 `((t (:height unspecified))))))
  (setq org-todo-keywords
        '((sequence
           "SOMEDAY(s)"
           "TODO(t)"
           "BLOCKED(b)"
           "NEXT(n)"
           "WAITING(w)"
           "INPROGRESS(i)"
           "REVIEW(r)"
           "|"
           "DONE(d)"
           "CANCELLED(c)")))
  (setq org-todo-keyword-faces
        '(("SOMEDAY" :foreground "#eeeeee" :background "#444444" :weight bold :underline t)
          ("TODO" :foreground "#fc9d03" :weight bold :underline t)
          ("BLOCKED" :foreground "#eee" :background "#ff0000" :weight bold :underline t)
          ("NEXT" :foreground "#0098dd" :weight bold :underline t)
          ("WAITING" :foreground "#bf3cc6" :weight bold :underline t)
          ("INPROGRESS" :foreground "#fc5603" :weight bold :underline t)
          ("REVIEW" :foreground "#964B00" :weight bold :underline t)
          ("DONE" :foreground "#50a14f" :weight bold :underline t)
          ("CANCELLED" :foreground "#fc0303" :weight bold :underline t)))
  (setq org-capture-templates
     '(("t" "Todo" entry (file+headline "~/Dropbox/Work/todo.org" "Work")
        "* TODO %?\n %i\n %a")
       ("s" "Standup" entry (file+headline "~/Dropbox/Work/standup.org" "Announcements")
        "* TODO %?")
       ("l" "Leads Meeting" entry (file+headline "~/Dropbox/Work/leads.org" "Notes")
        "* %?")
       ("d" "DevOps Note" entry (file+headline "~/Dropbox/Work/devops/notes.org" "Agenda")
        "* %?")
       ("o" "One on One" entry (file+headline "~/Dropbox/Work/one_on_one.org" "Agenda")
        "* %?")))
  :custom
  (org-odt-preferred-output-format "docx")
  (org-ellipsis "↴")
  (org-tags-column -77)
  (org-directory "~/Dropbox")
  (org-fontify-whole-heading-line nil)
  (org-confirm-babel-evaluate nil)
  ;; (org-latex-inputenc-alist '(("utf8" . "utf8x")))
  (org-edit-src-content-indentation 0)
  (org-refile-targets '((("~/Dropbox/notes.org"
                          "~/Dropbox/Work/todo.org"
                          "~/Dropbox/Work/devops.org"
                          "~/Dropbox/Work/projects") . (:maxlevel . 2))))
  (org-startup-indented 1)
  (org-agenda-files `("~/Org/calendars"
                      "~/Dropbox/notes.org"
                      "~/Dropbox/Work/todo.org"
                      "~/Dropbox/Work/notes.org"
                      "~/Dropbox/Work/onboarding.org"
                      "~/Dropbox/Work/projects"
                      "~/Dropbox/Work/devops"
                      "~/Dropbox/Bethlehem/notes.org"
                      "~/Dropbox/Bethlehem/classes/fall_2021/GREK_5205/assignments.org"
                      "~/Dropbox/Bethlehem/classes/fall_2021/THEO_5565/assignments.org"
                      "~/Dropbox/Bethlehem/classes/fall_2021/THEO_5581/assignments.org"
                      "~/Dropbox/Bethlehem/classes/fall_2021/THEO_5565/worksheets.org"))
  (org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done))
  (calendar-date-style 'iso)
  (calendar-mode-line-format nil)
  (calendar-date-display-form calendar-iso-date-display-form)
  (org-log-done 'time)
  :general
  (:keymaps 'org-mode-map
   :states 'normal
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line)
  (:keymaps 'org-mode-map
   :states '(normal insert)
   "M-t" 'insert-timed-note)
  (:keymaps 'org-mode-map
   :states 'insert
   "M-k" 'org-metaup
   "M-j" 'org-metadown)
  (:states 'normal
   :keymaps 'org-mode-map
   :prefix "SPC"
   "a c" 'calendar)
  (:states 'normal
   "M-c" 'org-store-link)
  (:states 'normal
   :prefix ","
   :keymaps 'org-mode-map
   "i" 'org-clock-in
   "o" 'org-clock-out
   "a" 'org-archive-subtree
   "c" 'org-ctrl-c-ctrl-c
   "e" 'org-export-dispatch
   "f" 'org-footnote-action
   "RET" 'org-open-at-point
   "l" 'org-insert-link
   "s" 'org-schedule
   "p" 'org-priority
   "d" 'org-deadline
   "g" 'org-goto
   "t" 'org-todo
   "r" 'org-refile
   "." 'org-time-stamp))

(use-package org-drill
  :after org
  :commands org-drill)

(use-package org-agenda
  :straight nil
  :commands (org-agenda-list org-agenda)
  :custom
  (org-agenda-timegrid-use-ampm t)
  :config
  (setq org-agenda-custom-commands
        '(("A" "Daily agenda and top priority tasks"
           ((tags-todo "*"
                       ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                        (org-agenda-skip-function
                         `(org-agenda-skip-entry-if
                           'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                        (org-agenda-block-separator nil)
                        (org-agenda-overriding-header "Imortant tasks without a date\n")))
            (agenda "" ((org-agenda-span 1)
                        (org-deadline-warning-days 0)
                        (org-agenda-block-separator nil)
                        (org-agenda-schedule-past-days 0)
                        (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                        (org-agenda-format-face "%A %-e %B %Y")
                        (org-agenda-overriding-header "\nToday's agenda\n")))
            (agenda "" ((org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "+1d")
                        (org-agenda-span 3)
                        (org-deadline-warning-days 0)
                        (org-agenda-block-separator nil)
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                        (org-agenda-overriding-header "\nNext three days\n")))
            (agenda "" ((org-agenda-time-grid nil)
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "+4d")
                        (org-agenda-span 14)
                        (org-agenda-show-all-dates nil)
                        (org-deadline-warning-days 0)
                        (org-agenda-block-separator nil)
                        (org-agenda-entry-types '(:deadline))
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                        (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")))))))
  :general
  (:states 'normal
   :prefix "SPC"
   "a A" 'org-agenda-list
   "a a" 'org-agenda))

(use-package org-journal
  :commands (org-journal-new-entry)
  :custom
  (org-journal-dir "~/Dropbox/Work/journal/")
  (org-journal-file-type 'daily)
  (org-journal-file-format "%Y%m%d.org")
  :general
  (:states 'normal
   :prefix "SPC"
   "o j" 'org-journal-new-entry))

 ;;; Addition functionality/functions
(add-hook 'org-agenda-finalize-hook #'color-org-agenda)

(defun jh/org-mode-hook ()
  (setq fill-column 80)
  (auto-fill-mode 1)
  (org-toggle-pretty-entities))

(add-hook 'org-mode-hook #'jh/org-mode-hook)

 ;;; Install external packages
(use-package org-tree-slide
  :after org)

(use-package ob-restclient
  :after org)

(use-package org-edna
  :after org
  :config
  (org-edna-mode))

(use-package org-superstar
  :after org
  :custom
  (org-hide-leading-stars t)
  (org-superstar-special-todo-items t)
  :hook (org-mode . org-superstar-mode))
