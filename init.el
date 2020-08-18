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
;; (defvar orig-gc-cons-threshold gc-cons-threshold)
;; (setq gc-cons-threshold 80000000)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(require 'bootstrap "~/.emacs.d/bootstrap.el")

;;; TODO: figure out better model between centralized bindings module
;;; and module specific imports of packages which have keys bound
;;; in the bindings module.  This creates a gross implicit dependency

(defvar jh/config nil)


(defconfig jh/config
  :color-theme          (:env "EMACS_COLOR_THEME"         :default "leuven")
  :color-theme-package  (:env "EMACS_COLOR_THEME_PACKAGE" :default "leuven-theme")
  :font                 (:env "EMACS_FONT"                :default "mono")
  :font-size            (:env "EMACS_FONT_SIZE"           :default "12")
  :highlight-line       nil
  :modules (core
            evil
            appearance
            modeline
            buffers
            completion
            erc
            bindings
            code
            git
            org
            icons
            dired
            lisp
            haskell
            rust
            eshell
            term
            terraform
            search
            python
            docker
            rss
            clojure
            php
            web
            ansible
            mail
            kubernetes
            personal
            spotify
            work))

;; (setq gc-cons-threshold orig-gc-cons-threshold)

(provide 'init)
;;; init.el ends here
