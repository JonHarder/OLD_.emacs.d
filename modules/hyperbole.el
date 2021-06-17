;;; hyperbole --- The general knowledge access and organization tool.

;;; Commentary:
;;;; This defines all configuration related to the various parts of hyperbole,
;;;; most notably, my attempt at evil bindints for koutline mode


;;; Code:
(require 'use-package)
(require 'evil)

(defun modules/hyperbole--load (config)
  "Configuration for my hyperbole module, as determined by CONFIG."
  (use-package hyperbole
    :after evil
    :functions (kotl-mode:add-cell
                kotl-mode:transpose-cells
                kotl-mode:previous-cell
                kotl-mode:kill-contents
                kotl-mode:beginning-of-line
                kotl-mode:kill-line
                kotl-mode:move-beginning-of-line
                kotl-mode:move-end-of-line)
    :custom
    (hyrole-file-list (cons "~/.rolo.otl"
                            (append
                             (cddr (directory-files "~/Org" t "[^.]"))
                             (cddr (directory-files "~/notes" t "[^.]")))))
    (kotl-mode:refill-flag t)
    :config
    (defun jh/kotl-insert-cell-below ()
      (interactive)
      (kotl-mode:add-cell)
      (evil-insert-state))

    (defun jh/kotl-insert-cell-above ()
      (interactive)
      (kotl-mode:add-cell)
      (call-interactively #'kotl-mode:transpose-cells)
      (kotl-mode:previous-cell 2)
      (evil-insert-state))

    (defun jh/kotl-append-line ()
      (interactive)
      (kotl-mode:move-end-of-line)
      (evil-insert-state))

    (defun jh/kotl-insert-line ()
      (interactive)
      (kotl-mode:move-beginning-of-line)
      (evil-insert-state))

    (defun jh/kotl-delete-contents ()
      (interactive)
      (kotl-mode:kill-contents 1))

    (defun jh/kotl-change-contents ()
      (interactive)
      (kotl-mode:kill-contents 1)
      (evil-insert-state))

    (defun jh/kotl-change-whole-line ()
      (interactive)
      (kotl-mode:beginning-of-line)
      (kotl-mode:kill-line)
      (evil-insert-state))

    (defun jh/kotl-change-end-of-line ()
      (interactive)
      (kotl-mode:kill-line)
      (evil-insert-state))

    (defun jh/kotl-delete-whole-line ()
      (interactive)
      (kotl-mode:beginning-of-line)
      (kotl-mode:kill-line 1))

    (defun jh/kotl-change-region ()
      (interactive)
      (call-interactively #'kotl-mode:kill-region)
      (evil-insert-state))

    (defun jh/kotl-change-word ()
      (interactive)
      (call-interactively #'kotl-mode:kill-word)
      (evil-insert-state))

    (evil-define-key 'normal kotl-mode-map
        [C-return] #'kotl-mode:add-cell
        "gg" #'kotl-mode:beginning-of-buffer
        "gc" #'kotl-mode:goto-cell
        ">>" #'kotl-mode:demote-tree
        "<<" #'kotl-mode:promote-tree
        "dat" #'kotl-mode:kill-tree
        "dd" #'jh/kotl-delete-whole-line
        "dac" #'jh/kotl-delete-contents
        "cac" #'jh/kotl-change-contents
        "cc" #'jh/kotl-change-whole-line
        "cw" #'jh/kotl-change-word
        "dw" #'kotl-mode:kill-word
        "C" #'jh/kotl-change-end-of-line
        "D" #'kotl-mode:kill-line
        "G" #'kotl-mode:end-of-buffer
        "0" #'kotl-mode:move-beginning-of-line
        "^" #'kotl-mode:move-beginning-of-line
        "$" #'kotl-mode:move-end-of-line
        "l" #'kotl-mode:forward-char
        "h" #'kotl-mode:backward-char
        "j" #'kotl-mode:next-line
        "k" #'kotl-mode:previous-line
        "o" #'jh/kotl-insert-cell-below
        "O" #'jh/kotl-insert-cell-above
        "e" #'kotl-mode:forward-word
        "w" #'kotl-mode:forward-word
        "b" #'kotl-mode:backward-word
        "A" #'jh/kotl-append-line
        "I" #'jh/kotl-insert-line)
    (evil-define-key 'insert kotl-mode-map
        (kbd "DEL") #'kotl-mode:delete-backward-char
        (kbd "M-o") #'jh/kotl-insert-cell-below)
    (evil-define-key 'visual kotl-mode-map
        "d" #'kotl-mode:kill-region
        "c" #'jh/kotl-change-region)))
