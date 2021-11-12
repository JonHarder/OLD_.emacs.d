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
(require 'use-package)
(load-file "~/.emacs.d/ext_lisp/how-do-i.el")
(require 'contrib "~/.emacs.d/contrib.el")

(defun text-scale-reset ()
  "Reset the text scale back to configuration default.

This is determined by `jh/font-size'"
  (interactive)
  (text-scale-adjust 0))


(defun delete-this-file ()
  "Delete the file of the current window, and it's associated buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (when (y-or-n-p (format "Delete %s? " (buffer-file-name buf)))
      (delete-file (buffer-file-name buf))
      (kill-buffer buf))))
            
(use-package general
  :config
  (general-evil-setup t)
  (general-define-key
   "M-[" 'evil-jump-backward
   "M-]" 'evil-jump-forward)
  (general-define-key
   :states 'normal
   :keymaps 'special-mode-map
   "TAB" 'forward-button
   "q" 'quit-window)
  (general-define-key
   :states 'normal
   "M-." 'xref-find-definitions
   "M-," 'xref-pop-marker-stack
   "M-v" 'jh/paste-from-mac-clipboard
   "C-t" 'transpose-chars
   "=" 'balance-windows
   "M-o" 'org-open-at-point-global)
  (general-define-key
   :states '(normal insert)
   "M-v" 'jh/paste-from-mac-clipboard)
  (general-define-key
   :states 'visual
   "M-c" 'jh/copy-to-mac-clipboard)
  (general-define-key
   :keymaps 'prog-mode-map
   :states 'insert
   "RET" 'newline)
  (general-define-key
   :states 'normal
   :keymaps 'occur-mode-map
   "e" 'occur-edit-mode)
  (general-define-key
   :prefix ","
   :states 'normal
   "m" 'normal-mode)
  (general-define-key
   :prefix "SPC"
   :states 'visual
   "n" 'narrow-to-region)
  (general-define-key
   :prefix "SPC"
   :states 'normal
   "x" 'execute-extended-command
   ";" 'eval-expression
   "`" 'shell-command
   "!" 'async-shell-command
   "RET" 'org-capture
   "ESC" 'evil-ex-nohighlight
   "TAB" 'switch-to-most-recent-buffer

   "1" #'delete-other-windows
   "2" #'split-window-below
   "3" #'split-window-right
   "0" #'delete-window

   "a =" 'calc
   "a c" 'calendar
   "a i" 'ielm
   "a p" 'appt-add
   "a t" 'org-todo-list

   "b i" 'ibuffer
   "b l" 'jh/switch-buffer-left
   "b r" 'jh/switch-buffer-right
   "b R" 'rename-buffer
   "b s" 'buffer-move-out-of-side-window
   "b t" 'window-toggle-side-windows

   "c c" 'jh/find-config
   "c l" 'select-theme
   "c r" 'jh/reload-config
   "c t" 'osx/toggle-dark-mode

   "d" #'kill-this-buffer
   "D" #'evil-delete-buffer

   "e d" 'eval-defun
   "e ;" 'eval-expression

   "f c" 'find-calendar
   "f i" 'find-shell-config
   "f ." 'jh/find-config
   "f f" 'find-file
   "f s" 'save-buffer
   "f t" 'find-todo-file
   "f m" 'jh/find-module
   "f p" 'ffap
   "f r" 'rename-current-buffer-file

   ;; frame based commands
   "f o" 'other-frame
   "f 0" 'delete-frame
   "f 2" 'make-frame
   "f x" 'delete-this-file
   "h f" 'helpful-callable
   "h k" 'helpful-key
   "h v" 'helpful-variable
   "h m" 'describe-mode
   "h p" 'describe-package
   "h i" 'info

   ;;; i
   "i l" 'imenu-list-smart-toggle

   ;;; k
   "k g" 'jh/work-git
   "k f" 'jh/work-find-file
   "k l" 'jh/jira-link

   "l r" 'lsp-ui-peek-find-references
   "l d" 'lsp-ui-peek-find-definitions
   "l n" 'flycheck-next-error
   "l p" 'flycheck-previous-error

   "n d" 'narrow-to-defun
   "n n" 'narrow-to-defun
   "n w" 'widen

   "p c" 'compile
   "p u" 'straight-use-package
   "p f" 'project-find-file
   "p p" 'project-switch-project

   "q" #'kill-emacs

   ;; r
   "r" #'winner-redo

   ;;; v
   "=" 'text-scale-increase
   "-" 'text-scale-decrease))

(use-package winner
  :after general
  :config
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "u" 'winner-undo))

(use-package how-do-i
  :straight nil
  :config
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "s g" 'how-do-i-google
   "s d" 'how-do-i-ddg
   "s o" 'how-do-i-so))

(use-package tab-bar
  :after general
  :config
  (tab-bar-mode 1)
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "t t" 'tab-bar-switch-to-tab
   "t n" 'tab-bar-switch-to-next-tab
   "t p" 'tab-bar-switch-to-prev-tab
   "t k" 'tab-bar-close-tab
   "t c" 'tab-bar-new-tab
   "t r" 'tab-bar-rename-tab))

(defun jh/reload-config ()
  "Evaluate current settings of Emacs configuration."
  (interactive)
  (message "reloading config...")
  (parse-profile)
  (load-file "~/.emacs.d/init.el")
  (update-frame-font-size jh/font-size)
  (message "reloading config...done!"))

(defun find-init-file ()
  "Open your init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun find-todo-file ()
  "Go to my tasks file."
  (interactive)
  (find-file "~/Dropbox/Work/todo.org"))

(defun infer-shell-config-file ()
  "Determine the shell configuration file according to environment variable `SHELL'."
  (let ((shell (getenv "SHELL")))
    (cond
     ((string-equal "/usr/local/bin/fish" shell)
      "~/.config/fish/config.fish")
     ((string-equal "/bin/bash" shell)
      (if (string-equal window-system "ns")
          "~/.bash_profile"
        "~/.bashrc")))))

(defun find-shell-config ()
  "Open shell configuration file."
  (interactive)
  (find-file (infer-shell-config-file)))

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
  "Take any occurence of `~' and replace whole line with that char."
  (interactive)
  (delete-minibuffer-contents)
  (insert "~/"))

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

(defun jh/smart-ace-window (swap-p)
  "Function like `ace-window`, but if universal argument SWAP-P supplied, use `ace-swap-window`."
  (interactive "P")
  (call-interactively
   (if swap-p
       #'ace-swap-window
     #'ace-window)))

(defun open-diary ()
  "Open the diary file as determined by `diary-file`."
  (interactive)
  (find-file diary-file))

(put 'narrow-to-region 'disabled nil)

(use-package ace-window
  :commands ace-window
  :after general
  :custom
  (aw-keys '(?a ?s ?h ?t ?n ?e ?o ?i))
  (aw-background t)
  (aw-scope 'frame)
  (aw-ignore-current t)
  :bind
  ("C-x o" . 'jh/smart-ace-window)
  :general
  (:states 'normal
   :prefix "SPC"
   "w" 'jh/smart-ace-window)
  :config
  (face-spec-set 'aw-leading-char-face '((t (:foreground "red" :height 3.0)))))

(use-package imenu-list
  :commands (imenu-list))

(defun jh/split-right-switch-buffer ()
  "Make a vertical split, then prompt for a buffer to display in the new split."
  (interactive)
  (evil-window-vsplit)
  (evil-window-right 1)
  (call-interactively #'switch-to-buffer))

(defun switch-to-most-recent-buffer ()
  "Switch to the most recently access buffer."
  (interactive)
  (switch-to-buffer nil))

(use-package beacon
  :after general
  :config
  (beacon-mode 1)
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "z" #'beacon-blink)
  (general-define-key
   "C-<return>" #'beacon-blink))

(use-package calendar
  :straight nil
  :after evil
  :commands 'calendar
  :general
  (:keymaps 'calendar-mode-map
   :states '(normal motion)
   "l" 'calendar-forward-day
   "h" 'calendar-backward-day
   "j" 'calendar-forward-week
   "k" 'calendar-backward-week))

(eval-after-load 'spotify
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "m RET" #'spotify-toggle-play
   "m n" #'spotify-next-track
   "m p" #'spotify-previous-track))

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

(provide 'bindings)
;;; bindings.el ends here
