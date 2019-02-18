;;; bindings --- Summary
;; set up key bindings

;;; Commentary:
;; This sets up general which handles setting key bindings.
;; As a general design philosopy, the bindings follow a scheme
;; introduced by spacemacs.  SPACE will start a command, the next
;; letter defines the logical object the command interacts with
;; eg.g 'w' for window commands.
;; and the next (final?) letter performs the operation.
;; which-key is used to provide feedback in the minibufer as to
;; which keys will perform which actions in mid chord.

;;; Code:

(use-package which-key
  :ensure t
  :config
  (which-key-mode))


(defun find-init-file ()
  "Open your init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))


(defun jh/term ()
  (interactive)
  (ansi-term "/usr/local/bin/fish"))

(defun jh/find-module (module)
  "Open the MODULE, using completion from availble modules."
  (interactive (list (completing-read "Module: "
                        (directory-files (expand-file-name "~/.emacs.d/modules/")
                          nil
                          "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))))
  (find-file (format "~/.emacs.d/modules/%s" module)))


(use-package ace-window
  :ensure t
  :commands (ace-window))


(use-package general
  :ensure t
  :config
  (general-create-definer space-leader :prefix "SPC")
  (space-leader
    :keymaps 'normal
    ";" 'eval-expression
    "1" 'shell-command
    ;; find manipulation
    "f f" 'find-file
    "f s" 'save-buffer
    "f i" 'find-init-file
    "f m" 'jh/find-module
    ;; buffer manipulation
    "b d" 'evil-delete-buffer
    ;; help
    "h f" 'describe-function
    "h k" 'describe-key
    "h v" 'describe-variable
    "h m" 'describe-mode
    "h p" 'describe-package
    ;; narrowing
    "n r" 'narrow-to-region
    "n d" 'narrow-to-defun
    "n w" 'widen
    ;; window manipulation
    "w /" 'evil-window-vsplit
    "w -" 'evil-window-split
    "w w" 'ace-window
    "w m" 'delete-other-windows
    "w c" 'delete-window
    ;; git stuff (using magit)
    "g s" 'magit-status
    ;; project stuff
    "p f" 'projectile-find-file
    ;; jumpin an stuff
    "j d" 'xref-find-definitions
    ;; evalin stuff
    "e e" 'eval-expression
    ;; searchin stuff
    "s g" 'how-do-i-google
    "s s" 'how-do-i
    "s d" 'how-do-i-ddg
    "s o" 'how-do-i-so
    ;; org
    "o c" 'org-ctrl-c-ctrl-c
    "o a" 'org-archive-subtree
    "o t" 'org-todo
    "o o" 'org-open-at-point
    ;; font stuff
    "=" 'jh/increse-font-size
    "-" 'jh/decrease-font-size
    ;; "applications"
    "a a" 'org-agenda
    "a t" 'jh/term
    "a d" 'dired))


(provide 'bindings)
;;; bindings.el ends here
