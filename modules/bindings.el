;; This sets up general which handles setting key bindings.
;; As a general design philosopy, the bindings follow a scheme
;; introduced by spacemacs.  SPACE will start a command, the next
;; letter defines the logical object the command interacts with
;; eg.g 'w' for window commands.
;; and the next (final?) letter performs the operation.
;; which-key is used to provide feedback in the minibufer as to
;; which keys will perform which actions in mid chord.

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
  "Open the diary file as determined by `diary-file`''"
  (interactive)
  (find-file diary-file))

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

  (defun switch-to-most-recent-buffer ()
    (interactive)
    (switch-to-buffer nil))

  (use-package general
    :config
    (general-evil-setup t)
    (general-create-definer space-leader :prefix "SPC")
    (general-define-key
     :states 'normal
     :keymaps 'occur-mode-map
     "e" 'occur-edit-mode)
    (general-define-key
     :states 'normal
     "/" 'consult-line
     "M-v" 'jh/paste-from-mac-clipboard
     "C-t" 'transpose-chars
     :states 'insert
     "M-v" 'jh/paste-from-mac-clipboard
     :states 'visual
     "M-c" 'jh/copy-to-mac-clipboard)
    (space-leader
      :keymaps 'normal
      "SPC" 'execute-extended-command
      ";" 'eval-expression
      "1" 'shell-command
      "RET" 'eshell
      "ESC" 'evil-ex-nohighlight
      "TAB" 'switch-to-most-recent-buffer
  
      "a" '(:ignore t :which-key "applications")
      "a a" 'org-agenda
      "a t" 'vterm
      ;; "a t" 'eshell
      "a d" 'dired-jump
      "a c" 'calc
      "a i" 'jh/erc
      "a s" 'scratch

      "b" '(:ignore t :which-key "buffers")
      "b b" 'consult-buffer
      "b i" 'ibuffer
      "b d" 'evil-delete-buffer
      "b o" 'consult-imenu
      "b D" 'kill-current-buffer
      "b l" 'jh/switch-buffer-left
      "b r" 'jh/switch-buffer-right
      "b R" 'rename-buffer
      "b p" 'popwin:display-buffer
  
      "c" '(:ignore t :which-key "configuration")
      "c c" 'jh/find-config
      "c l" 'jh/load-theme
      "c r" 'jh/reload-config

      ;;; d
      "d" '(:ignore t :which-key "display")
      "d c" 'display-fill-column-indicator-mode
      "d d" 'jh/load-dark-theme
      "d l" 'jh/load-light-theme

      "e" '(:ignore t :which-key "eval")
      "e e" 'eval-last-sexp
      "e ;" 'eval-expression
  
      "f" '(:ignore t :which-key "files")
      "f c" 'find-shell-config
      "f d" 'open-diary
      "f f" 'find-file
      "f s" 'save-buffer
      "f t" 'find-todo-file
      "f m" 'jh/find-module
      "f p" 'popwin:find-file
  
      "g" '(:ignore t :which-key "git")
      "g s" 'magit-status
  
      "h" '(:ignore t :which-key "help")
      "h f" 'helpful-callable
      "h k" 'helpful-key
      "h v" 'helpful-variable
      "h m" 'describe-mode
      "h p" 'describe-package
      "h a" 'consult-apropos

      ;;; i
      "i" '(:ignore t :which-key "imenu")
      "i i" 'consult-imenu
      "i l" 'imenu-list-smart-toggle
  
      "j" '(:ignore t :which-key "jumping")
      ;; "j d" 'dumb-jump-go-other-window
      "j d" 'lsp-find-definition
      "j w" 'avy-goto-word-1

      ;;; k
      "k" '(:ignore t :which-key "kipsu")
      "k g" 'jh/work-git
      "k f" 'jh/work-find-file

      "l" '(:ignore t :which-key "lsp")
      "l r" 'lsp-ui-peek-find-references
      "l d" 'lsp-ui-peek-find-definitions
      "l n" 'flycheck-next-error
      "l p" 'flycheck-previous-error
      "l l" 'flycheck-list-errors

      "m" '(:ignore t :which-key "music")
      "m p" 'spotify-toggle-play
      "m s" 'spotify-track-search
      "m l" 'spotify-my-playlists
      "m d" 'spotify-select-device
  
      "n" '(:ignore t :which-key "narrowing")
      "n d" 'narrow-to-defun
      "n n" 'narrow-to-defun
      "n w" 'widen
  
      "o" '(:ignore t :which-key "org")
      "o c" 'org-ctrl-c-ctrl-c
      "o a" 'org-archive-subtree
      "o b" 'jh/org-src-block
      "o t" 'org-todo
      "o e" 'org-export-dispatch
      "o o" 'org-open-at-point
      "o s s" 'org-schedule
      "o s d" 'org-deadline
      "o l" 'org-insert-link
      "o p" 'org-priority
      "o '" 'org-edit-special

      "p" '(:ignore t :which-key "project")
      "p p" 'projectile-switch-project
      "p f" 'projectile-find-file
      "p /" 'rg
      "p c" 'compile

      ;; q
      ;; r

      "s" '(:ignore t :which-key "searching")
      "s g" 'how-do-i-google
      "s s" 'how-do-i
      "s d" 'how-do-i-ddg
      "s o" 'how-do-i-so

      ;; t
      "t" '(:ignore t :which-key "tabs")
      "t t" 'tab-bar-switch-to-tab
      "t n" 'tab-bar-switch-to-next-tab
      "t p" 'tab-bar-switch-to-prev-tab
      "t k" 'tab-bar-close-tab
      "t c" 'tab-bar-new-tab
      "t r" 'tab-bar-rename-tab
      ;; u
      ;; v
  
      "w" '(:ignore t :which-key "windows")
      "w r"    'edwina-arrange
      "w n"    'edwina-select-next-window
      "w p"    'edwina-select-previous-window
      "w N"    'edwina-swap-next-window
      "w P"    'edwina-swap-previous-window
      "w %"    'edwina-dec-mfact
      "w ^"    'edwina-inc-mfact
      "w d"    'edwina-dec-nmaster
      "w i"    'edwina-inc-nmaster
      "w k"    'edwina-delete-window
      "w RET " 'edwina-zoom
      "w c"    'edwina-clone-window
      "w o"    'delete-other-windows

      ;;; x
      ;;; y
      ;;; z
  
  
      "=" 'text-scale-increase
      "-" 'text-scale-decrease)))
