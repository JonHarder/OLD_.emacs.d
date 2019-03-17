;;; init --- Summary
;; my emacs configuration

;;; Commentary:
;; bootstrap the package system, configure the module loader
;; load modules

;;; Code:
;; (package-initialize)
(require 'bootstrap "~/.emacs.d/bootstrap.el")

(defvar jh/config nil
  "The configuration object used to store user settings.

Meant to be used alongside `defconfig', which will take a series of keyword
arguments and generate a valid configuration.

This will later be used in `jh/config-init', which will load the settings.")


(defconfig jh/config
  :color-theme-light doom-molokai
  :color-theme-dark doom-molokai
  :font "Fira Code"
  :font-size 17
  :modules '(core
             completion
             bindings
             git
             jorg
             appearance
             icons
             modeline
             evil
             elm
             eshell
             term
             search
             python
             clojure
             php
             web
             work))


(jh/config-init jh/config)


(provide 'init)
;;; init.el ends here
