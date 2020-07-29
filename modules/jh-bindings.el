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

(defun find-todo-file ()
  "Go to my tasks file."
  (interactive)
  (find-file "~/Org/todo.org"))


(defun find-shell-config ()
  "Open shell configuration file."
  (interactive)
  (find-file "~/.config/fish/config.fish"))


(defun new-buffer (&optional name)
  "Create a new buffer, called NAME."
  (interactive)
  (let ((buf (generate-new-buffer (or name "untitled"))))
    (switch-to-buffer buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    buf))


(defun jh/switch-buffer-right (buf)
  "Split window to the right, and switch to buffer."
  (interactive "bBuffer: ")
  (evil-window-vsplit)
  (evil-window-right 1)
  (switch-to-buffer buf))


(defun jh/switch-buffer-left (buf)
  "Split window to the left, and switch to buffer."
  (interactive "bBuffer: ")
  (evil-window-vsplit)
  (switch-to-buffer buf))


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

;; (defalias 'jump-to-function 'counsel-imenu)
(defalias 'jump-to-definition 'xref-find-definitions)

(defun jh/find-config ()
  "Open the configuration file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun jh/occur-other-window ()
  "Run `occur', then switch to generated pane."
  (interactive)
  (call-interactively 'occur)
  (other-window 1))

(defun jh/dired-open-in-current-directory ()
  "Open `dired' in current directory."
  (interactive)
  (dired "."))
  

(defun modules/bindings--load (config)
  "Configure all things key bindings using CONFIG."
  (put 'narrow-to-region 'disabled nil)
  (use-package which-key
    :config
    (which-key-mode))

  (use-package ace-window
    :commands (ace-window))

  (straight-use-package 'imenu-list)

  (defun jh/split-right-switch-buffer ()
    (interactive)
    (evil-window-vsplit)
    (evil-window-right 1)
    (call-interactively #'switch-to-buffer))

  (use-package general
    :config
    (general-evil-setup t)
    (general-create-definer space-leader :prefix "SPC")
    (general-define-key
     :states 'normal
     ;; "/" 'swiper
     "M-v" 'jh/paste-from-mac-clipboard
     :states 'insert
     "M-v" 'jh/paste-from-mac-clipboard
     :states 'visual
     "M-c" 'jh/copy-to-mac-clipboard)
    (space-leader
      :keymaps 'normal
      "SPC" 'execute-extended-command
      ";" 'eval-expression
      "1" 'shell-command
      "RET" 'org-capture
      "ESC" 'evil-ex-nohighlight
      "/" 'jh/occur-other-window
  
      "a" '(:ignore t :which-key "applications")
      "a a" 'org-agenda
      "a t" 'vterm
      "a d" 'jh/dired-open-in-current-directory
      "a c" 'calc
      "a e" 'eshell-toggle
      "a i" 'jh/erc

      "b" '(:ignore t :which-key "buffers")
      "b b" 'switch-to-buffer
      "b i" 'ibuffer
      "b d" 'kill-current-buffer
      "b D" 'evil-delete-buffer
      "b l" 'jh/switch-buffer-left
      "b r" 'jh/switch-buffer-right
  
      "c" '(:ignore t :which-key "configuration")
      "c r" 'jh/reload-config
      "c f" 'jh/find-config
  
      ;;; d

      "e" '(:ignore t :which-key "eval")
      "e e" 'eval-last-sexp
      "e ;" 'eval-expression
  
      "f" '(:ignore t :which-key "files")
      "f c" 'find-shell-config
      "f f" 'find-file
      "f s" 'save-buffer
      "f i" 'find-init-file
      "f t" 'find-todo-file
      "f m" 'jh/find-module
  
      "g" '(:ignore t :which-key "git")
      "g s" 'magit-status
  
      "h" '(:ignore t :which-key "help")
      "h f" 'helpful-callable
      "h k" 'helpful-key
      "h v" 'helpful-variable
      "h m" 'describe-mode
      "h p" 'describe-package
      "h a" 'apropos

      ;;; i
  
      "j" '(:ignore t :which-key "jumping")
      "j d" 'dumb-jump-go-other-window
      "j c" 'avy-goto-char
      "j l" 'imenu-list-smart-toggle

      ;;; k
      "k" '(:ignore t :which-key "kipsu")
      "k g" 'jh/kipsu-git

      "m" '(:ignore t :which-key "meetings")
      "m s" 'jh/standup-today
  
      "n" '(:ignore t :which-key "narrowing")
      "n d" 'narrow-to-defun
      "n n" 'narrow-to-defun
      "n w" 'widen
  
      "o" '(:ignore t :which-key "org")
      "o c" 'org-ctrl-c-ctrl-c
      "o a" 'org-archive-subtree
      "o b" 'org-insert-structure-template
      "o t" 'org-todo
      "o e" 'org-export-dispatch
      "o o" 'org-open-at-point
      "o s s" 'org-schedule
      "o s d" 'org-deadline
      "o l" 'org-insert-link
      "o p" 'org-priority

      "p" '(:ignore t :which-key "project")
      "p p" 'projectile-switch-project
      "p f" 'projectile-find-file
      "p /" 'rg
      "p c" 'compile

      "s" '(:ignore t :which-key "searching")
      "s g" 'how-do-i-google
      "s s" 'how-do-i
      "s d" 'how-do-i-ddg
      "s o" 'how-do-i-so
  
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

      ;;; x
      ;;; y
      ;;; z
  
  
      "=" 'text-scale-increase
      "-" 'text-scale-decrease)))
  

(provide 'jh-bindings)
;;; jh-bindings.el ends here
