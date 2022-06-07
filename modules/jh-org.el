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

;; (eval-after-load 'org
;;   (require 'oc))

(setq org-cite-global-bibliography '("/Users/jharder/Dropbox/bibiography/references.bib"))

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
  (set-face-attribute 'org-checkbox-statistics-done nil
                      :box '(:line-width (2 . 2)
                                         :color "green3"
                                         :inherit org-done))
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

  (setq org-todo-keywords
        '((sequence
           "SOMEDAY(s)"
           "TODO(t)"
           "BLOCKED(b)"
           "NEXT(n)"
           "WAITING(w)"
           "INPROGRESS(i)"
           "REVIEW(r)"
           "MERGE(m)"
           "|"
           "DONE(d)"
           "CANCELLED(c)")))
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
  (org-directory "~/Dropbox")
  (org-fontify-whole-heading-line nil)
  (org-confirm-babel-evaluate nil)
  (org-hide-leading-stars nil)
  (org-hide-emphasis-markers t)
  ;; (org-latex-inputenc-alist '(("utf8" . "utf8x")))
  (org-edit-src-content-indentation 0)
  (org-refile-targets '((("~/Dropbox/notes.org"
                          "~/Dropbox/Work/todo.org"
                          "~/Dropbox/Work/devops.org"
                          "~/Dropbox/Work/projects") . (:maxlevel . 2))))
  ;; (org-startup-indented 1)
  (org-startup-indented nil)
  (org-agenda-files `("~/Org/calendars.org"
                      "~/Dropbox/calendar.org"
                      "~/Dropbox/notes.org"
                      "~/Dropbox/Work/todo.org"
                      "~/Dropbox/Work/notes.org"
                      "~/Dropbox/Work/onboarding.org"
                      "~/Dropbox/Work/projects"
                      "~/Dropbox/Work/projects.org"
                      "~/Dropbox/Work/devops"
                      "~/Dropbox/Work/team.org"
                      "~/Dropbox/Bethlehem/notes.org"
                      "~/Dropbox/Bethlehem/classes/winter_2021/GREK_5206"
                      "~/Dropbox/Bethlehem/classes/winter_2021/THEO_5582"
                      "~/Dropbox/Bethlehem/classes/spring_2022/THEO_5567"))
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
                       ((org-agenda-overriding-header "Imortant tasks without a date\n")
                        (org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                        (org-agenda-skip-function
                         `(org-agenda-skip-entry-if
                           'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                        (org-agenda-block-separator nil)))

            (tags-todo "*"
                       ((org-agenda-overriding-header "\nBlocked tasks\n")
                        (org-agenda-time-grid nil)
                        (org-agenda-block-separator nil)
                        (org-agenda-skip-function '(org-agenda-skip-entry-if  'nottodo '("BLOCKED")))))

            (agenda "" ((org-agenda-overriding-header "\nToday's agenda\n")
                        (org-agenda-span 1)
                        (org-deadline-warning-days 0)
                        (org-agenda-block-separator nil)
                        (org-agenda-schedule-past-days 0)
                        (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                        (org-agenda-format-face "%A %-e %B %Y")))

            (agenda "" ((org-agenda-overriding-header "\nNext three days\n")
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "+1d")
                        (org-agenda-span 3)
                        (org-deadline-warning-days 0)
                        (org-agenda-block-separator nil)
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))

            (agenda "" ((org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")
                        (org-agenda-time-grid nil)
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "+4d")
                        (org-agenda-span 14)
                        (org-agenda-show-all-dates nil)
                        (org-deadline-warning-days 0)
                        (org-agenda-block-separator nil)
                        (org-agenda-entry-types '(:deadline))
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))))
          ("T" . "Team + Name tag searches")
          ("Tr" tags "+ryan")
          ("Tc" tags "+chee")
          ("Tm" tags "+minh")
          ("N" todo "NEXT")))
           
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
  "Hook to run when loading an org mode buffer."
  (visual-line-mode 1)
  (olivetti-mode -1)
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

(defun reset-window-dividers (orig-load-theme &rest args)
  (message "resetting window dividers")
  (mapc 'disable-theme custom-enabled-themes)
  (let ((res (apply orig-load-theme args)))
    (dolist (face '(window-divider
                    window-divider-first-pixel
                    window-divider-last-pixel))
      (face-spec-reset-face face)
      (set-face-foreground face (face-attribute 'default :background)))
    (set-face-background 'fringe (face-attribute 'default :background))
    (highlight-indent-guides-mode -1)
    (highlight-indent-guides-mode +1)
    res))

(advice-add 'load-theme :around #'reset-window-dividers)

(with-eval-after-load 'org
  (face-spec-set 'org-level-1 '((t (:height 1.9))))
  (face-spec-set 'org-level-2 '((t (:height 1.6))))
  (face-spec-set 'org-level-3 '((t (:height 1.3))))
  (face-spec-set 'org-level-4 '((t (:height 1.0)))))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config
  (modify-all-frames-parameters
   '((right-divider-width . 20)
     (internal-border-width . 20)))
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background))
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka")
  :custom
  (org-modern-todo-faces
   '(("TODO" :background "yellow" :foreground "black")
     ("NEXT" :background "#c4cfff" :foreground "blue")
     ("WAITING" :background "purple" :foreground "white")
     ("INPROGRESS" :background "blue" :foreground "white")))
  (line-spacing 0.1)
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis "…")
  (org-agenda-block-separator ?-)
  (org-agenda-toggle-time-grid
   '((daily today required-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────"))

;; (use-package svg-tag-mode
;;   :after org
;;   :hook (org-mode . svg-tag-mode)
;;   :init
;;   (defconst date-re (rx (= 4 num) "-" (= 2 num) "-" (= 2 num)))
;;   (defconst time-re (rx (= 2 num) ":" (= 2 num)))
;;   (defconst day-re (rx (= 3 letter)))
;;   ;; (defconst date-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))
;;   (defconst date-time-re (rx
;;                           (? (group (literal day-re)))
;;                           (? space)
;;                           (? (group (literal time-re)))))
;;   :config
;;   (defun svg-progress-percent (value)
;;     (svg-image (svg-lib-concat
;;                 (svg-lib-progress-bar (/ (string-to-number value) 100.0)
;;                                       nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
;;                 (svg-lib-tag (concat value "%")
;;                              nil :stroke 0 :margin 0))
;;                :ascent 'center))
;;   (defun svg-progress-count (value)
;;     (let* ((seq (mapcar #'string-to-number (split-string value "/")))
;;            (count (float (car seq)))
;;            (total (float (cadr seq))))
;;       (svg-image (svg-lib-concat
;;                   (svg-lib-progress-bar (/ count total) nil
;;                                         :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
;;                   (svg-lib-tag value nil
;;                                :stroke 0 :margin 0))
;;                  :ascent 'center)))
;;   :custom
;;   (svg-tag-tags
;;    `(
;;      ;; Org tags
;;      (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
;;      (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))
     
;;      ;; Task priority
;;      ("\\[#[A-Z]\\]" . ((lambda (tag)
;;                           (svg-tag-make tag :face 'org-priority
;;                                         :beg 2 :end -1 :margin 0))))
     
;;      ;; Progress
;;      ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
;;                                          (svg-progress-percent (substring tag 1 -2)))))
;;      ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
;;                                        (svg-progress-count (substring tag 1 -1)))))
     
;;      ;; TODO / DONE
;;      ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
;;      ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))
;;      ("INPROGRESS" . ((lambda (tag) (svg-tag-make "IN-PROGRESS" :face 'org-footnote :inverse t :margin 0))))
     
     
;;      ;; Citation of the form [cite:@Knuth:1984]
;;      ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
;;                                        (svg-tag-make tag
;;                                                      :inverse t
;;                                                      :beg 7 :end -1
;;                                                      :crop-right t))))
;;      ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
;;                                                 (svg-tag-make tag
;;                                                               :end -1
;;                                                               :crop-left t))))
     
     
;;      ;; Active date (with or without day name, with or without time)
;;      (,(format "\\(<%s>\\)" date-re) .
;;       ((lambda (tag)
;;          (svg-tag-make tag :beg 1 :end -1 :margin 0))))
;;      (,(format "\\(<%s \\)%s>" date-re date-time-re) .
;;       ((lambda (tag)
;;          (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
;;      (,(format "<%s \\(%s>\\)" date-re date-time-re) .
;;       ((lambda (tag)
;;          (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))
     
;;      ;; Inactive date  (with or without day name, with or without time)
;;      (,(format "\\(\\[%s\\]\\)" date-re) .
;;       ((lambda (tag)
;;          (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
;;      (,(format "\\(\\[%s \\)%s\\]" date-re date-time-re) .
;;       ((lambda (tag)
;;          (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
;;      (,(format "\\[%s \\(%s\\]\\)" date-re date-time-re) .
;;       ((lambda (tag)
;;          (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date)))))))

(provide 'jh-org)
;;; jh-org.el ends here
