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
(require 'bootstrap "~/.emacs.d/bootstrap.el")

;;; TODO: figure out better model between centralized bindings module
;;; and module specific imports of packages which have keys bound
;;; in the bindings module.  This creates a gross implicit dependency

(defvar jh/config nil)

(defconfig jh/config
  :font                 (:env "EMACS_FONT"      :default "mono")
  :font-size            (:env "EMACS_FONT_SIZE" :default "12")
  :theme                (:env "EMACS_THEME"     :default "modus")
  :profile              t
  :highlight-line       nil
  :scale-org-headings   t
  :modules ( core
            evil
            ;; hyperbole
            modeline
            appearance
            window
            buffers
            completion
            irc
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
            bindings
            ;;; TESTING MODULE
            prose))

(provide 'init)
;;; init.el ends here
