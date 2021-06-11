;;; init --- Summary
;; my emacs configuration

;;; Commentary:
;; This is the starting point of this Emacs configuration.
;; The First step is to bootstrap the system via bootstrap.el.
;; This defines key, low level settings, like:
;;
;;  - package management (using straight.el)
;;  - methods and macros related to the module system
;;  - any environment variables required for core functionality
;;  - the `defconfig' macro, a declarative expression meant to
;;    define, at a glance, the major settings at play
;;
;; For each module defined below, there exists a file in the `modules'
;; folder called 'MODULE_NAME.el'
;; and among other things, a function called `modules/MODULE_NAME--load'
;; which takes a config object.  This config is roughly what you see
;; being passed to `defconfig'.  This allows each module to have the
;; entirety of this configuration at its disposal without relying on
;; some hardcoded global config.

;;; Code:
;; (package-initialize)

;;;; as of emacs 28.0.50 emacs-lisp/byte-run.el macro `define-obsolete-function-alias has
;;; changed signature and broken among other things, `eshell'
;;;; overwriting it here (hopefully) to revert to the original version
;; patch to emacs@28.0.50
;; https://www.reddit.com/r/emacs/comments/kqd9wi/changes_in_emacshead2828050_break_many_packages/
;; (defmacro define-obsolete-function-alias ( obsolete-name current-name
;;                                            &optional when docstring)
;;   "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.
;; \(define-obsolete-function-alias \\='old-fun \\='new-fun \"22.1\" \"old-fun's doc.\")
;; is equivalent to the following two lines of code:
;; \(defalias \\='old-fun \\='new-fun \"old-fun's doc.\")
;; \(make-obsolete \\='old-fun \\='new-fun \"22.1\")
;; WHEN should be a string indicating when the function was first
;; made obsolete, for example a date or a release number.
;; See the docstrings of `defalias' and `make-obsolete' for more details."
;;   (declare (doc-string 4)
;;            (advertised-calling-convention)
;;            ;; New code should always provide the `when' argument
;;            (obsolete-name current-name when &optional docstring) "23.1")
;;   `(progn
;;      (defalias ,obsolete-name ,current-name ,docstring)
;;      (make-obsolete ,obsolete-name ,current-name ,when)))

(setq gc-cons-threshold (* 50 1000 1000))
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(require 'bootstrap "~/.emacs.d/bootstrap.el")

;;; TODO: figure out better model between centralized bindings module
;;; and module specific imports of packages which have keys bound
;;; in the bindings module.  This creates a gross implicit dependency

(defvar jh/config nil)

;;; Note, humanoid themes are nice
(defconfig jh/config
  :font                 (:env "EMACS_FONT"                :default "mono")
  :font-size            (:env "EMACS_FONT_SIZE"           :default "12")
  :theme-package        modus-themes
  :light-theme          modus-operandi
  :dark-theme           modus-vivendi
  :highlight-line       t
  :modules (core
            evil
            hyperbole
            modeline
            appearance
            window
            buffers
            completion
            ;; erc
            code
            git
            org
            icons
            dired
            lisp
            racket
            ;; haskell
            ;; rust
            go
            eshell
            term
            terraform
            search
            python
            docker
            rss
            ;; clojure
            php
            web
            ;; ansible
            mail
            ;; kubernetes
            personal
            spotify
            ;; deft
            ;; zettelkasten
            work
            bindings))

;; (setq gc-cons-threshold orig-gc-cons-threshold)

(provide 'init)
;;; init.el ends here
