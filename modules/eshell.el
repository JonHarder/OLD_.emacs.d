(require 'dash)
(require 's)

(use-package eshell-up
  :config
  (defalias 'eshell/up #'eshell-up)
  (defalias 'eshell/pk #'eshell-up-peek))


;;; some programs don't play nice with eshell, for these, we can use ansi-term automatically
;;; use
;;;  'eshell-visual-commands
;;; or
;;;  'eshell-visual-subcommands
;;; to update which programs will use this shell

(defvar pretty-eshell-funcs nil
  "List of `pretty-eshell-section' to enable.")

(defvar pretty-eshell-sep " | "
  "String delimits each `pretty-eshell-section'.")

(defvar pretty-eshell-section-delim " "
  "String delimits icons and their text.")

(defvar pretty-eshell-header ""
  "Initial string composing the eshell prompt.")

(defvar pretty-eshell-prompt-string " "
  "Prompt string, must match builtin `eshell-prompt-regexp'.")

(setq eshell-prompt-regexp " "
      eshell-cmpl-ignore-case t)

(defvar pretty-eshell-prompt-num 0
  "Prompt number for current eshell session.")

;;; Core

(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))

(defmacro pretty-eshell-section (name icon form &rest props)
  "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
  ;; Roundabout way to handle case that
  ;; 1. Form is a variable and
  ;; 2. That variable might not be defined/initialized
  ;; Eg. pyvenv-virtualenv-name not loaded until pyvenv-workon
  `(setq ,name
         (lambda ()
           (when (or (and (symbolp (quote ,form))
                          (bound-and-true-p ,form))
                     (and (not (symbolp (quote ,form)))
                          ,form))
             (-> ,icon
                (concat pretty-eshell-section-delim ,form)
                (with-face ,@props))))))

(defun pretty-eshell--acc (acc x)
  "Used to accumulate shell sections into ACC, using X."
  (--if-let (funcall x)
      (if (s-blank? acc)
          it
        (s-concat acc pretty-eshell-sep it))
    acc))

(defun pretty-eshell-prompt-func ()
  "Value for `eshell-prompt-function'."
  (concat pretty-eshell-header
          (-reduce-from 'pretty-eshell--acc "" pretty-eshell-funcs)
          "\n"
          pretty-eshell-prompt-string))

;;; define the sections

;; Git Branch


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
(defun eshell/e (file)
  "Shorthand command to open FILE."
  (find-file file))


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
      (magit-status))
     (t (shell-command-to-string (string-join (cons "git" command) " "))))))

(defalias 'eshell/gs (lambda () (eshell/git "status")))

(defun modules/eshell--load (config)
  "Load configuration for eshell using CONFIG."
  (straight-use-package 'dash-functional)

  (defun jh/eshell-mode-setup ()
    (company-mode nil))

  (setq eshell-prefer-lisp-functions nil)

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

  (pretty-eshell-section
   esh-git
   "\xe907"  ; 
   (magit-get-current-branch)
   '(:foreground "#8D6B94"))
  
  ;; Python Virtual Environment
  (pretty-eshell-section
   esh-python
   "\xe928"  ; 
   pyvenv-virtual-env-name)
  
  ;; Time
  (pretty-eshell-section
   esh-clock
   "\xf017"  ; 
   (format-time-string "%I:%M %p" (current-time))
   '(:foreground "forest green"))
  
  (pretty-eshell-section
   esh-dir
   "\xf016"  ;
   (abbreviate-file-name (eshell/pwd))
   '(:foreground "#268bd2" :bold bold :underline t))

  (setq pretty-eshell-funcs
        (list esh-dir esh-git esh-python esh-clock))
  
  (setq eshell-prompt-function
        'pretty-eshell-prompt-func)

  (add-hook 'eshell-mode-hook
    (lambda ()
      (define-key eshell-mode-map
        (kbd "<tab>")
        (lambda ()
          (interactive)
          (completion-at-point))))))
