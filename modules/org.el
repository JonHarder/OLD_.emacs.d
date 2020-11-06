(defun modules/org--load (config)
  "Load configuration related to org using CONFIG."
  (straight-use-package 'org)

  ;; programmatic org workflow triggers and actions
  (use-package org-edna
    :config
    (org-edna-mode))

  (require 'org-habit)
  (require 'org-agenda)
  (require 'ob-python)
  (require 'ob-php (concat user-emacs-directory "ob-php.el"))
  (setq org-fontify-whole-heading-line t
        org-confirm-babel-evaluate nil)
  (setq-default
   org-agenda-files '("~/Org" "~/zettelkasten")
   org-src-fontify-natively t
   org-hide-emphasis-markers nil
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
           "TODO(t)"
           "WAITING(w)"
           "INPROGRESS(i)"
           "|"
           "DONE(d)"
           "CANCELLED(c)"))
        org-todo-keyword-faces
        '(("TODO" :foreground "#dba12a" :weight normal :underline t)
          ("WAITING" :foreground "#9f7efe" :weight normal :underline t)
          ("INPROGRESS" :foreground "#0098dd" :weight normal :underline t)
          ("DONE" :foreground "#50a14f" :weight normal :underline t)
          ("CANCELLED" :foreground "#ff6480" :weight normal :underline t))
        org-log-done 'time))
