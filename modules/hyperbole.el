;;; hyperbole --- The general knowledge access and organization tool.

;;; Commentary:
;;;; This defines all configuration related to the various parts of hyperbole,
;;;; most notably, my attempt at evil bindints for koutline mode


;;; Code:
(require 'straight)
(defun modules/hyperbole--load (config)
  "Configuration for my hyperbole module, as determined by CONFIG."
  (straight-use-package 'hyperbole)
  (require 'hyperbole)
  (require 'hyrolo)
  (setq hyrolo-file-list (cons "~/.rolo.otl" (append
                                                (cddr (directory-files "~/Org" t "[^.]"))
                                                (cddr (directory-files "~/notes" t)))))
  
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
  
  (with-eval-after-load 'evil
    (evil-define-key 'normal kotl-mode-map
        (kbd "g g") #'kotl-mode:beginning-of-buffer
        (kbd "g c") #'kotl-mode:goto-cell
        (kbd "> >") #'kotl-mode:demote-tree
        (kbd "< <") #'kotl-mode:promote-tree
        (kbd "d d") #'kotl-mode:kill-tree
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
        "A" (lambda ()
                (interactive)
                (kotl-mode:move-end-of-line)
                (evil-insert-state))
        "I" (lambda ()
                (interactive)
                (kotl-mode:move-beginning-of-line)
                (evil-insert-state)))
    (evil-define-key 'insert kotl-mode-map
        (kbd "DEL") #'kotl-mode:delete-backward-char)
    (evil-define-key 'visual kotl-mode-map
        "d" #'kotl-mode:kill-region)))
