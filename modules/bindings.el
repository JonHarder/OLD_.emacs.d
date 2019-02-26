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


(use-package ace-window
  :ensure t
  :commands (ace-window))


(use-package ivy
  :after (counsel general evil)
  :ensure t
  :init
  (setq ivy-use-virtual-buffers 1
        enable-recursive-minibuffers t)
  :config
  (ivy-mode 1))


(use-package ivy-rich
  :after ivy
  :ensure t
  :init
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-path-style 'abbrev
        ivy-format-function #'ivy-format-function-line)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode))


(defun jh/copy-to-mac-clipboard (from to)
  "Copy the selected region starting at FROM and ending at TO to the clipboard."
  (interactive
   (if (use-region-p)
     (list (region-beginning) (region-end))
     (error "Region not selected!")))
  (shell-command-on-region from to "pbcopy")
  (evil-exit-visual-state)
  (message "copied to clipboard"))


(use-package general
  :ensure t
  :config
  (general-create-definer space-leader :prefix "SPC")
  (general-define-key
   :states 'normal
   "/" 'swiper
   :states 'visual
   "M-c" 'jh/copy-to-mac-clipboard)
  (space-leader
    :keymaps 'normal
    "SPC" 'counsel-M-x
    ";" 'eval-expression
    "1" 'shell-command
    ;; find manipulation
    "f f" 'find-file
    "f s" 'save-buffer
    "f i" 'find-init-file
    "f m" 'jh/find-module
    ;; buffer manipulation
    "b b" 'ivy-switch-buffer
    "b i" 'ibuffer
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
    "p p" 'projectile-switch-project
    "p f" 'projectile-find-file
    ;; find in file
    "i" 'counsel-imenu
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
    "=" 'text-scale-increase
    "-" 'text-scale-decrease
    ;; "applications"
    "a a" 'org-agenda
    "a t" 'jh/term
    "a d" 'dired))


(provide 'bindings)
;;; bindings.el ends here
