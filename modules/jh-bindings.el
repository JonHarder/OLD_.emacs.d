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
(defun jh/reload-config ()
  "Evaluate current settings of Emacs configuration."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun find-init-file ()
  "Open your init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))


(defun jh/term ()
  "Open my terminal."
  (interactive)
  (ansi-term (getenv "SHELL")))


(defun jh/expand-directory (dir)
  "Expand the given DIR to the list of all of its files."
  (interactive)
  (directory-files (expand-file-name dir)
    nil
    "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))


(defun jh/find-module (module)
  "Open the MODULE, using completion from availble modules."
  (interactive (list (completing-read "Module: "
                        (jh/expand-directory "~/.emacs.d/modules/"))))
  (find-file (format "~/.emacs.d/modules/%s" module)))


(defun jh/paste-from-mac-clipboard ()
  "Insert the contents of the clipboard into the current buffer."
  (interactive)
  (insert (shell-command-to-string "pbpaste")))


(defun jh/copy-to-mac-clipboard (from to)
  "Copy the selected region starting at FROM and ending at TO to the clipboard."
  (interactive
   (if (use-region-p)
     (list (region-beginning) (region-end))
     (error "Region not selected!")))
  (shell-command-on-region from to "pbcopy")
  (evil-exit-visual-state)
  (message "copied to clipboard"))

(defalias 'jump-to-function 'counsel-imenu)
(defalias 'jump-to-definition 'xref-find-definitions)

(defun modules/bindings--load (config)
  "Configure all things key bindings using CONFIG."
  (use-package which-key
    :config
    (which-key-mode))

  (use-package ace-window
    :commands (ace-window))

  (straight-use-package 'imenu-list)

  (use-package general
    :config
    (general-evil-setup t)
    (general-create-definer space-leader :prefix "SPC")
    (general-define-key
     :states 'normal
     "/" 'swiper
     "M-v" 'jh/paste-from-mac-clipboard
     :states 'insert
     "M-v" 'jh/paste-from-mac-clipboard
     :states 'visual
     "M-c" 'jh/copy-to-mac-clipboard)
    (space-leader
      :keymaps 'normal
      "SPC" 'counsel-M-x
      ";" 'eval-expression
      "1" 'shell-command
  
      "f" '(:ignore t :which-key "files")
      "f f" 'find-file
      "f s" 'save-buffer
      "f i" 'find-init-file
      "f m" 'jh/find-module
  
      "b" '(:ignore t :which-key "buffers")
      "b b" 'ivy-switch-buffer
      "b i" 'ibuffer
      "b d" 'evil-delete-buffer
  
      "h" '(:ignore t :which-key "help")
      "h f" 'describe-function
      "h k" 'describe-key
      "h v" 'describe-variable
      "h m" 'describe-mode
      "h p" 'describe-package
  
      "c" '(:ignore t :which-key "configuration")
      "c r" 'jh/reload-config
  
      "n" '(:ignore t :which-key "narrowing")
      "n r" 'narrow-to-region
      "n d" 'narrow-to-defun
      "n w" 'widen
  
      "w" '(:ignore t :which-key "windows")
      "w /" 'evil-window-vsplit
      "w -" 'evil-window-split
      "w w" 'ace-window
      "w m" 'delete-other-windows
      "w c" 'delete-window
      "w l" 'evil-window-right
      "w h" 'evil-window-left
      "w k" 'evil-window-up
      "w j" 'evil-window-down
      "w H" 'evil-window-move-far-left
      "w L" 'evil-window-move-far-right
      "w K" 'evil-window-move-far-up
      "w J" 'evil-window-move-far-down
      "w =" 'balance-windows
      "w t f" 'flycheck-list-errors
  
      "g" '(:ignore t :which-key "git")
      "g s" 'magit-status
  
      "p" '(:ignore t :which-key "projectile")
      "p p" 'projectile-switch-project
      "p f" 'projectile-find-file
      "p /" 'counsel-ag
  
      "j" '(:ignore t :which-key "jumping")
      "j d" 'xref-find-definitions-other-window
      "j f" 'jump-to-function
      "j l" 'imenu-list-smart-toggle
  
      "e" '(:ignore t :which-key "eval")
      "e e" 'eval-last-sexp
  
      "s" '(:ignore t :which-key "searching")
      "s g" 'how-do-i-google
      "s s" 'how-do-i
      "s d" 'how-do-i-ddg
      "s o" 'how-do-i-so
  
      "o" '(:ignore t :which-key "org")
      "o c" 'org-ctrl-c-ctrl-c
      "o a" 'org-archive-subtree
      "o t" 'org-todo
      "o e" 'org-export-dispatch
      "o o" 'org-open-at-point
      "o s s" 'org-schedule
      "o s d" 'org-deadline
      "o l" 'org-insert-link
      "o p" 'org-priority

      "m" '(:ignore t :which-key "meetings")
      "m s" 'jh/standup-today
  
      "=" 'text-scale-increase
      "-" 'text-scale-decrease
  
      "a" '(:ignore t :which-key "applications")
      "a a" 'org-agenda
      "a t" 'jh/term
      "a d" 'dired
      "a c" 'calc
      "a e" 'eshell)))

(provide 'jh-bindings)
;;; jh-bindings.el ends here
