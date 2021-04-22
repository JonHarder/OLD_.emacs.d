(defun modules/org--load (config)
  "Load configuration related to org using CONFIG."
  ;; programmatic org workflow triggers and actions
  (use-package org-edna
    :config
    (org-edna-mode))

  ;;; org roam
  ;; (use-package org-roam
  ;;   :hook
  ;;   (after-init . org-roam-mode)
  ;;   :custom
  ;;   (org-roam-directory "~/zettelkasten")
  ;;   :bind (:map org-roam-mode-map
  ;;               (("C-c n l" . org-roam)
  ;;                ("C-c n f" . org-roam-find-file)
  ;;                ("C-c n g" . org-roam-graph))
  ;;               :map org-mode-map
  ;;               (("C-c n i" . org-roam-insert))
  ;;               (("C-c n I" . org-roam-insert-immediate))))


  (require 'org-habit)
  (require 'org-agenda)
  (require 'ob-python)
  (require 'ob-php (concat user-emacs-directory "ob-php.el"))
  (setq org-fontify-whole-heading-line t
        org-confirm-babel-evaluate nil)
  (setq org-agenda-files '("~/Org/calendars" "~/Org/today.org"))
  (setq calendar-date-style 'iso
        ;; calendar-mark-diary-entries-flag nil
        calendar-mode-line-format nil
        calendar-date-display-form calendar-iso-date-display-form)

  ;; (add-hook 'diary-list-entries-hook 'diary-sort-entries t)
  ;; (add-to-list 'auto-mode-alist '("diary\\'" . diary-mode))

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

  (add-to-list 'org-modules 'org-habit t)

  (use-package org-tree-slide)

  (use-package ob-mermaid
    :defer 5)

  (use-package ob-restclient
    :defer 5)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (sql . t)
     (mermaid . t)
     (calc . t)
     (restclient . t)
     (haskell t)
     (python . t)
     (php . t)
     (js . t)))

  
  (use-package org-superstar
    :demand t
    :custom
    (org-hide-leading-stars t)
    (org-superstar-special-todo-items t)
    :hook (org-mode . org-superstar-mode))
  
  (setq org-todo-keywords
        '((sequence
           "SOMEDAY(s)"
           "TODO(t)"
           "NEXT(n)"
           "WAITING(w)"
           "INPROGRESS(i)"
           "REVIEW(r)"
           "|"
           "DONE(d)"
           "CANCELLED(c)"))
        org-todo-keyword-faces
        '(("SOMEDAY" :foreground "#eeeeee" :background "#444444" :weight bold :underline t)
          ("TODO" :foreground "#fc9d03" :weight bold :underline t)
          ("NEXT" :foreground "#0098dd" :weight bold :underline t)
          ("WAITING" :foreground "#bf3cc6" :weight bold :underline t)
          ("INPROGRESS" :foreground "#fc5603" :weight bold :underline t)
          ("REVIEW" :foreground "#964B00" :weight bold :underline t)
          ("DONE" :foreground "#50a14f" :weight bold :underline t)
          ("CANCELLED" :foreground "#fc0303" :weight bold :underline t))
        org-log-done 'time))
