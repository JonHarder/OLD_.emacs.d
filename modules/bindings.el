;;; bindings -- Sets most of all configured keybindings. -*- lexical-binding: t -*-

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
(require 'contrib "~/.emacs.d/contrib.el")
(require 'evil)
(require 'diary-lib)
(require 'org)
(require 'use-package)
(require 'winner)

(defun jh/reload-config ()
  "Evaluate current settings of Emacs configuration."
  (interactive)
  (message "reloading config...")
  (load-file "~/.emacs.d/init.el")
  (message "reloading config...done!"))

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

(defun find-calendar (calendar)
  "Find the calendar file called CALENDAR from available calendars."
  (interactive (list (completing-read "Calendar: "
                                      (jh/expand-directory "~/Org/calendars/"))))
  (find-file (format "~/Org/calendars/%s" calendar)))


(defun new-buffer (&optional name)
  "Create a new buffer, called NAME."
  (interactive)
  (let ((buf (generate-new-buffer (or name "untitled"))))
    (switch-to-buffer buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    buf))


(defun jh/switch-buffer-right (buf)
  "Split window to the right, and switch to buffer BUF."
  (interactive "bBuffer: ")
  (evil-window-vsplit)
  (evil-window-right 1)
  (switch-to-buffer buf))


(defun jh/switch-buffer-left (buf)
  "Split window to the left, and switch to buffer BUF."
  (interactive "bBuffer: ")
  (evil-window-vsplit)
  (switch-to-buffer buf))


(defun jh/term ()
  "Open my terminal."
  (interactive)
  (ansi-term (getenv "SHELL")))


(defun jh/find-module (module)
  "Open the MODULE, using completion from availble modules."
  (interactive (list (completing-read "Module: "
                                      (jh/expand-directory "~/.emacs.d/modules/"))))
  (find-file (format "~/.emacs.d/modules/%s" module)))


(defun jh/paste-from-mac-clipboard ()
  "Insert the contents of the clipboard into the current buffer."
  (interactive)
  (insert (shell-command-to-string "pbpaste")))

(defun minibuffer-replace-with-home ()
  (interactive)
  (delete-minibuffer-contents)
  (insert "~/"))

(defun jh/kill-this-buffer ()
  "Delete current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

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

(defun jh/smart-ace-window ()
  "If only two windows are present, jump to the other one, otherwise use `ace-window'."
  (interactive)
  (if (= 2 (count-windows))
      (other-window 1)
    (call-interactively #'ace-window)))

(defun jh/org-src-block (mode)
  "Insert an Org src block of type MODE."
  (interactive "sMode: ")
  (org-insert-structure-template "src")
  (insert mode)
  (org-edit-special))

(defun open-diary ()
  "Open the diary file as determined by `diary-file`."
  (interactive)
  (find-file diary-file))

(defun modules/bindings--load (config)
  "Configure all things key bindings using CONFIG."
  (put 'narrow-to-region 'disabled nil)
  (use-package which-key
    :config
    (which-key-mode))

  (use-package ctrlf
    :config
    (ctrlf-mode +1))

  (use-package ace-window
    :commands (ace-window)
    :custom
    (aw-keys '(?a ?s ?h ?t ?n ?e ?o ?i))
    (aw-background t)
    (aw-scope 'frame)
    (aw-ignore-current t)
    :bind
    ("C-x o" . 'ace-window)
    :config
    (face-spec-set 'aw-leading-char-face '((t (:foreground "red" :height 3.0)))))

  (use-package imenu-list)

  (defun jh/split-right-switch-buffer ()
    (interactive)
    (evil-window-vsplit)
    (evil-window-right 1)
    (call-interactively #'switch-to-buffer))

  (defun switch-to-most-recent-buffer ()
    (interactive)
    (switch-to-buffer nil))

  (use-package beacon
    :config
    (beacon-mode 1))

  (defun rename-current-buffer-file ()
    "Renames current buffer and file it is visiting."
    (interactive)
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
        (if (not (and filename (file-exists-p filename)))
           (error "Buffer '%s' is not visiting a file!" name)
          (let ((new-name (read-file-name "New name: " filename)))
              (when (get-buffer new-name)
                (error "A buffer named '%s' already exists!" new-name))
              (rename-file filename new-name 1)
              (rename-buffer new-name)
              (set-visited-file-name new-name)
              (set-buffer-modified-p nil)
              (message "File '%s' successfully renamed to '%s'"
                         name (file-name-nondirectory new-name))))))

  (use-package general
    :config
    (general-evil-setup t)
    (general-define-key
     :states 'normal
     "α" 'evil-append
     "Α" 'evil-append-line
     "ι" 'evil-insert
     "Ι" 'evil-insert-line
     "ο" 'evil-open-below
     "Ο" 'evil-open-above
     "/" 'ctrlf-forward-fuzzy
     "?" 'ctrlf-backward-fuzzy
     "M-v" 'jh/paste-from-mac-clipboard
     "C-t" 'transpose-chars
     "=" 'balance-windows
     "M-o" 'org-open-at-point-global
     :states 'insert
     "M-v" 'jh/paste-from-mac-clipboard
     :states 'visual
     "M-c" 'jh/copy-to-mac-clipboard)
    (general-define-key
     :states '(normal)
     :keymaps 'magit-refs-mode-map
     "x" 'magit-delete-thing)
    (general-define-key
     :states 'insert
     :keymaps 'minibuffer-local-map
     "~" #'minibuffer-replace-with-home)
    (general-define-key
     :states '(normal insert)
     :keymaps 'vterm-mode-map
     "M-v" 'vterm-yank)
    (general-define-key
     :states 'normal
     :keymaps 'calendar-mode-map
     "i d" 'diary-insert-entry
     "H" 'calendar-backward-month
     "L" 'calendar-forward-month)
    (general-define-key
     :states 'normal
     :keymaps 'occur-mode-map
     "e" 'occur-edit-mode)
    (general-create-definer space-leader :prefix "SPC")
    ;;; NOTE: this does not work for now since the prefix: SPC j collides with jumping bindings
    ;; (space-leader
    ;;   :states 'normal
    ;;   :keymaps 'calendar-mode-map
    ;;   "j n" #'org-journal-new-entry
    ;;   "j d" #'org-journal-display-entry)
    (space-leader
      :keymaps 'normal
      "SPC" 'avy-goto-word-1
      "x" 'execute-extended-command
      ";" 'eval-expression
      "`" 'shell-command
      "!" 'async-shell-command
      "RET" 'org-capture
      "ESC" 'evil-ex-nohighlight
      "TAB" 'switch-to-most-recent-buffer
      ;; "/" 'consult-line
      "/" #'projectile-find-file

      "1" #'delete-other-windows
      "2" #'split-window-below
      "3" #'split-window-right
      "0" #'delete-window

      "a" '(:ignore t :which-key "Apps")
      "a =" 'calc
      "a a" 'org-agenda-list
      "a A" 'org-agenda
      "a c" 'calendar
      "a d" 'dired-jump
      ;; "a e" 'eshell
      "a g" 'gnus
      "a i" 'jh/erc
      "a s" 'scratch
      "a t" 'eshell
      "a m" 'check-mail
      "a M" 'notmuch
      "a w" 'writeroom-mode

      "b" '(:ignore t :which-key "Buffers")
      "b b" 'consult-buffer
      "b i" 'ibuffer
      "b o" 'consult-imenu
      "b l" 'jh/switch-buffer-left
      "b r" 'jh/switch-buffer-right
      "b R" 'rename-buffer
      "b s" 'buffer-move-out-of-side-window
      "b t" 'window-toggle-side-windows

      "c" '(:ignore t :which-key "Conf")
      "c c" 'jh/find-config
      "c l" 'select-theme
      "c r" 'jh/reload-config

      "d" #'evil-delete-buffer
      "D" #'jh/kill-this-buffer

      "e" '(:ignore t :which-key "Eval")
      "e e" 'eval-last-sexp
      "e ;" 'eval-expression

      "f" '(:ignore t :which-key "Files")
      "f c" 'find-calendar
      "f s" 'find-shell-config
      "f i" 'jh/find-config
      "f d" 'delete-file
      "f f" 'find-file
      "f s" 'save-buffer
      "f t" 'find-todo-file
      "f m" 'jh/find-module
      "f o" 'other-frame
      "f p" 'ffap
      "f r" '(rename-current-buffer-file :wk "Rename File")

      "g" '(:ignore t :which-key "Git")
      "g s" 'magit-status
      "g l" 'magit-log
      "g c" 'magit-commit
      "g f" 'magit-file-dispatch
      "g d" 'magit-dispatch

      "h" '(:ignore t :which-key "Help")
      "h f" 'helpful-callable
      "h k" 'helpful-key
      "h v" 'helpful-variable
      "h m" 'describe-mode
      "h p" 'describe-package
      "h a" 'consult-apropos

      ;;; i
      "i" '(:ignore t :which-key "Imenu")
      "i i" 'consult-imenu
      "i l" 'imenu-list-smart-toggle

      ;;; k
      "k" '(:ignore t :which-key "Kipsu")
      "k g" 'jh/work-git
      "k f" 'jh/work-find-file
      "k l" 'jh/jira-link

      "l" '(:ignore t :which-key "Lsp")
      "l r" 'lsp-ui-peek-find-references
      "l d" 'lsp-ui-peek-find-definitions
      "l n" 'flycheck-next-error
      "l p" 'flycheck-previous-error
      "l l" 'consult-flycheck

      "n" '(:ignore t :which-key "Narrow")
      "n d" 'narrow-to-defun
      "n n" 'narrow-to-defun
      "n w" 'widen

      "o" '(:ignore t :which-key "Org")
      "o c" 'org-ctrl-c-ctrl-c
      "o a" 'org-archive-subtree
      "o b" 'jh/org-src-block
      "o t" 'org-todo
      "o e" 'org-export-dispatch
      "o j" 'org-journal-new-entry
      "o o" 'org-open-at-point
      "o s s" 'org-schedule
      "o s d" 'org-deadline
      "o l" 'org-insert-link
      "o p" 'org-priority
      "o '" 'org-edit-special
      "o g" 'org-goto
      "o ." 'org-time-stamp

      "p" '(:ignore t :which-key "Projects")
      "p p" 'projectile-switch-project
      "p f" 'projectile-find-file
      "p t" 'parinfer-toggle-mode
      "p /" 'rg
      "p c" 'compile
      "p u" 'package-install

      "q" #'kill-emacs

      ;; r
      "r" #'winner-redo

      "s" '(:ignore t :which-key "Searching")
      "s g" '(how-do-i-google :wk "Google")
      "s d" '(how-do-i-ddg :wk "DuckDuckGo")
      "s o" '(how-do-i-so :wk "StackOverflow")
      "s b" '(how-do-i-bible :wk "Bible Gateway")

      ;; t
      "t" '(:ignore t :which-key "Tabs")
      "t t" '(tab-bar-switch-to-tab :wk "Switch")
      "t n" '(tab-bar-switch-to-next-tab :wk "Next")
      "t p" '(tab-bar-switch-to-prev-tab :wk "Prev")
      "t k" '(tab-bar-close-tab :wk "Klose")
      "t c" '(tab-bar-new-tab :wk "Create")
      "t r" '(tab-bar-rename-tab :wk "Rename")
      ;; u
      "u" #'winner-undo
      ;;; v

      ;;; w
      "w" #'ace-window

      ;;; x
      ;;; y
      "z" #'beacon-blink

      "=" 'text-scale-increase
      "-" 'text-scale-decrease)))
(provide 'bindings)
;;; bindings.el ends here
