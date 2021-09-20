;;; eshell --- Summary -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:
(defvar term-term-name nil)

;;; some programs don't play nice with eshell, for these, we can use ansi-term automatically
;;; use
;;;  'eshell-visual-commands
;;; or
;;;  'eshell-visual-subcommands
;;; to update which programs will use this shell

(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; custom completion
(defconst pcmpl-git-commands
  '("add" "bisect" "branch" "checkout" "clone"
    "commit" "diff" "fetch" "grep"
    "init" "log" "merge" "mv" "pull" "push" "rebase"
    "reset" "rm" "show" "status" "tag")
  "List of `git' commands.")

(defun pcomplete/git ()
  "Completion for `git'."
  (pcomplete-here* pcmpl-git-commands))

 ;;; ESHELL PROMPT HELPER FUNCTIONS
(defun jh/--join-paths (paths dir)
  "Take the list of PATHS, and join together with DIR at the end.

Takes into account if path contains the home ~ symbol."
  (cond
   ((and (null paths) (null dir))
    "/")
   ((and (null paths) (not (string-equal dir "~")))
    (format "/%s" dir))
   ((and (null paths) (string-equal dir "~"))
    "~")
   (t
    (let ((fmt (if (string-equal (car paths) "~")
                   "%s/%s"
                 "/%s/%s")))
      (format fmt (string-join paths "/") dir)))))


(defun jh/--split-path (path)
  "Split the string PATH using the directory seperator, /."
  (delete "" (split-string (abbreviate-file-name path) "/")))


(defun jh/--truncate-paths (paths)
  "Take list of PATHS and take first character of each except for last entry."
  (mapcar (lambda (s)
            (if (string-equal "." (substring s 0 1))
                (substring s 0 2)
              (substring s 0 1)))
          paths))

(defun shell-command-with-exit-code (command &rest args)
  "Execute shell command COMMAND with ARGS, returning list of (exit-code stdout)."
  (with-temp-buffer
    (condition-case _
        (list (apply 'call-process command nil (current-buffer) nil args)
              (string-trim (buffer-string)))
        (file-missing ""))))

(defun jh/eshell-prompt--git-branch ()
  "Get the git branch of working directory."
  (let* ((cmd-str "git branch | grep '^*' | awk '{ print $2 }'")
         (output (string-trim (shell-command-to-string cmd-str)))
         (in-repo? (not (s-starts-with? "fatal" output))))
    (if in-repo?
        output
      nil)))

(defun jh/eshell-prompt--compressed-pwd (dir)
  "Get compressed directory of current DIR."
  (interactive)
  (let* ((fragments (jh/--split-path dir))
         (first-chars (jh/--truncate-paths (butlast fragments))))
    (jh/--join-paths first-chars (car (last fragments)))))

(defun kill-eshell-window-on-close ()
  "Destroy the window when eshell process dies."
  (when (not (one-window-p))
    (delete-window)))

(advice-add 'eshell-life-is-too-much :after 'kill-eshell-window-on-close)

;;; custom functions
(defun eshell/e (&optional file)
  "Shorthand command to open FILE, defaults to current directory if not given."
  (let ((f (or file ".")))
    (find-file f)))


(defun eshell/back (&optional num)
  "Travel back NUM directories."
  (let ((num (or num 1)))
    (unless (integerp num)
      (error "Argument must be an integer"))
    (let* ((dots (make-list num ".."))
           (dir (contrib/str-join dots "/")))
      (eshell/cd dir))))

(defalias 'eshell/b #'eshell/back)


(defun eshell/git (&rest command)
  "Intercept 'git status' and run magit-status instead, run regular command line git command with COMMAND othrewise."
  (let ((sub-command (car command)))
    (cond
     ((string-equal sub-command "status")
      (magit-status-setup-buffer))
     (t (shell-command-to-string (string-join (cons "git" command) " "))))))

(defalias 'eshell/gs (lambda () (eshell/git "status")))

(use-package eshell-up
  :after eshell
  :commands (eshell-up eshell-up-peek)
  :config
  (defalias 'eshell/up #'eshell-up)
  (defalias 'eshell/pk #'eshell-up-peek))


(defun jh/eshell-prompt ()
  "Prompt for eshell."
  (let* ((branch (jh/eshell-prompt--git-branch))
         (success (= eshell-last-command-status 0))
         (status (propertize "➜ " 'face (if success
                                            'font-lock-type-face
                                          'error))))
    (concat
     status
     (propertize (jh/eshell-prompt--compressed-pwd default-directory) 'face 'font-lock-builtin-face)
     (unless (null branch)
       (concat
        (propertize " git:(" 'face 'font-lock-constant-face)
        (propertize branch 'face 'font-lock-keyword-face)
        (propertize ")" 'face 'font-lock-constant-face)))
     (propertize " $ " 'face 'font-lock-type-face))))

(use-package eshell
  :commands eshell
  :custom
  (eshell-banner-message "")
  (eshell-cmpl-cycle-completions t)
  (eshell-cmpl-dir-ignore "\\`\\(\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")
  (eshell-cmpl-ignore-case t)
  (eshell-highlight-prompt nil)
  (eshell-prefer-lisp-functions nil)
  (eshell-prompt-regexp ".* \$ ")
  (eshell-save-history-on-exit t)
  (eshell-prompt-function #'jh/eshell-prompt)
  :config
  (eval-after-load 'esh-opt
    '(progn
       (require 'em-prompt)
       (require 'em-term)
       (require 'em-cmpl)
       (setenv "PAGER" "cat")
       (add-hook 'eshell-mode-hook
                 (lambda () (eshell/export "TERM" "dumb")))))
  :general
  (:keymaps 'eshell-mode-map
   :states '(normal insert)
   "<tab>" 'completion-at-point)
  (:keymaps 'global
   :states '(normal insert)
   "C-c C-e" 'jh/eshell)
  (:states 'normal
   :prefix "SPC"
   "e e" 'jh/eshell))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

 ;; fish style autocompletion
(use-package esh-autosuggest
  :after eshell
  :hook (eshell-mode . esh-autosuggest-mode))

(defun jh/eshell ()
  "Open or close eshell window."
  (interactive)
  (if-let ((win (get-buffer-window "*eshell*")))
      (if (eq (selected-window) win)
          (delete-window win)
        (select-window win))
    (eshell)))

(provide 'eshell)
;;; eshell.el ends here
