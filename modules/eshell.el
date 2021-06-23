;;; eshell --- Summary

;;; Commentary:


;;; Code:
(require 'eshell)
(require 'dash)
(require 's)
(require 'use-package)
(require 'magit)
(require 'contrib "~/.emacs.d/contrib.el")
(require 'eshell)
(require 'straight)
(require 'company)

(defvar term-term-name nil)

;;; some programs don't play nice with eshell, for these, we can use ansi-term automatically
;;; use
;;;  'eshell-visual-commands
;;; or
;;;  'eshell-visual-subcommands
;;; to update which programs will use this shell

(use-package eshell-up
  :config
  (defalias 'eshell/up #'eshell-up)
  (defalias 'eshell/pk #'eshell-up-peek))


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

(defun modules/eshell--load (config)
  "Load configuration for eshell using CONFIG."
  (straight-use-package 'dash-functional)

  (defun jh/eshell-mode-setup ()
    (company-mode nil))

  (setq eshell-prefer-lisp-functions nil)

  (setq eshell-cmpl-cycle-completions t
        eshell-cmpl-ignore-case t
        eshell-save-history-on-exit t
        eshell-cmpl-dir-ignore "\\`\\(\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

  (eval-after-load 'esh-opt
    '(progn
       (require 'em-prompt)
       (require 'em-term)
       (require 'em-cmpl)
       (setenv "PAGER" "cat")
       (add-hook 'eshell-mode-hook
                 '(lambda () (eshell/export "TERM" "dumb")))))

  (use-package eshell-syntax-highlighting
    :after esh-mode
    :config
    (eshell-syntax-highlighting-global-mode +1))


  ;; fish style autocompletion
  (use-package esh-autosuggest
    :hook (eshell-mode . esh-autosuggest-mode)
    :hook (eshell-mode . jh/eshell-mode-setup))

  ;;; close eshell window when the process exits
  ;; (defun close-eshell-on-exit ()
  ;;   (when (not (one-window-p))
  ;;     (delete-window)))
  ;; (advice-add 'eshell-life-is-too-much :after 'close-eshell-on-exit)


  (setq eshell-banner-message "")

  ;;; ESHELL PROMPT HELPER FUNCTIONS
  (defun jh/--join-paths (paths dir)
    "Take the list of PATH, and join together with DIR at the end.

Takes into account if path contains the home ~ symbol."
    (cond
     ((and (null paths) (null dir))
      "/")
     ((and (null paths) (not (string-equal dir "~")))
      (format "/%s" dir))
     ((and (null paths) (string-equal dir "~"))
      "~")
     (t
      (let ((fmt (if (string-equal (first paths) "~")
                     "%s/%s"
                   "/%s/%s")))
        (format fmt (string-join paths "/") dir)))))


  (defun jh/--split-path (path)
    "Split the string PATH using the directory seperator, /."
    (delete "" (split-string (abbreviate-file-name path) "/")))


  (defun jh/--truncate-paths (paths)
    (mapcar (lambda (s)
              (if (string-equal "." (substring s 0 1))
                  (substring s 0 2)
                (substring s 0 1)))
            paths))

  (defun jh/eshell-prompt--git-branch ()
    (let* ((cmd-str "git branch | grep '^*' | awk '{ print $2 }'")
           (output (string-trim (shell-command-to-string cmd-str)))
           (in-repo? (not (s-starts-with? "fatal" output))))
      (if in-repo?
          output
        nil)))

  (defun jh/eshell-prompt--compressed-pwd (dir)
    (interactive)
    (let* ((fragments (jh/--split-path dir))
           (first-chars (jh/--truncate-paths (butlast fragments))))
      (jh/--join-paths first-chars (car (last fragments)))))
  

  (setq eshell-prompt-regexp ".* \$ "
        eshell-cmpl-ignore-case t
        eshell-highlight-prompt nil)

  (defun jh/eshell-prompt ()
    (let* ((color-success (if jh/dark-mode "#00ff00" "#228822"))
           (color-failure "red")
           (color-path (if jh/dark-mode "cyan" "#0088aa"))
           (color-default (if jh/dark-mode "white" "black"))
           (color-git (if jh/dark-mode "#5577ff" "#0022dd"))
           (color-git-branch "#cc3333")
           (branch (jh/eshell-prompt--git-branch))
           (status-color (if (= eshell-last-command-status 0)
                             color-success
                           color-failure)))
      (concat
       (with-face "➜ " `(:foreground ,status-color))
       (with-face (jh/eshell-prompt--compressed-pwd default-directory) `(:foreground ,color-path :weight bold))
       (unless (null branch)
         (concat
          (with-face " git:(" `(:foreground ,color-git :weight bold))
          (with-face branch `(:foreground ,color-git-branch :weight bold))
          (with-face ")" `(:foreground ,color-git :weight bold))))
       (with-face " $ " `(:foreground ,color-default)))))

  (setq eshell-prompt-function #'jh/eshell-prompt)
  
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map
                (kbd "<tab>")
                (lambda ()
                  (interactive)
                  (completion-at-point)))))

  (defun kill-eshell-window-on-close ()
    (when (not (one-window-p))
      (delete-window)))
  (advice-add 'eshell-life-is-too-much :after 'kill-eshell-window-on-close))

(provide 'eshell)
;;; eshell.el ends here
