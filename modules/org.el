(defun modules/org--load (config)
  "Load configuration related to org using CONFIG."
  (straight-use-package 'org)

  ;; programmatic org workflow triggers and actions
  (use-package org-edna
    :config
    (org-edna-mode))

  ;;; org roam
  (use-package org-roam
    :hook
    (after-init . org-roam-mode)
    :custom
    (org-roam-directory "~/zettelkasten")
    :bind (:map org-roam-mode-map
                (("C-c n l" . org-roam)
                 ("C-c n f" . org-roam-find-file)
                 ("C-c n g" . org-roam-graph))
           :map org-mode-map
                (("C-c n i" . org-roam-insert))
                (("C-c n I" . org-roam-insert-immediate))))


  (require 'org-habit)
  (require 'org-agenda)
  (require 'ob-python)
  (require 'ob-php (concat user-emacs-directory "ob-php.el"))
  (setq org-fontify-whole-heading-line t
        org-confirm-babel-evaluate nil)
  (setq-default
   org-agenda-files '("~/Org" "~/Org/projects" "~/zettelkasten")
   org-src-fontify-natively t
   org-hide-emphasis-markers t
   org-archive-location "~/Org/archive/%s.archive::"
   org-agenda-include-diary t
   org-agenda-timegrid-use-ampm t
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
     (restclient . t)
     (haskell t)
     (python . t)
     (php . t)
     (js . t)))

  
  (use-package org-superstar
    :demand t
    :custom
    (org-hide-leading-stars t)
    (org-superstar-special-todo-items nil)
    :hook (org-mode . org-superstar-mode))
  
  (setq org-todo-keywords
        '((sequence
           "SOMEDAY(s)"
           "TODO(t)"
           "NEXT(n)"
           "WAITING(w)"
           "INPROGRESS(i)"
           "|"
           "DONE(d)"
           "CANCELLED(c)"))
        org-todo-keyword-faces
        '(("SOMEDAY" :foreground "#eeeeee" :background "#444444" :weight normal :underline t)
          ("TODO" :foreground "#fc9d03" :weight normal :underline t)
          ("NEXT" :foreground "#0098dd" :weight normal :underline t)
          ("WAITING" :foreground "#bf3cc6" :weight normal :underline t)
          ("INPROGRESS" :foreground "#fc5603" :weight normal :underline t)
          ("DONE" :foreground "#50a14f" :weight normal :underline t)
          ("CANCELLED" :foreground "#fc0303" :weight normal :underline t))
        org-log-done 'time))
