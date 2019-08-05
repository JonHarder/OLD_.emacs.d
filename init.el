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

(straight-use-package 'doom-themes)
(straight-use-package 'kaolin-themes)


(defconfig jh/config
  :color-theme-light    doom-solarized-light
  :color-theme-dark     kaolin-aurora
  :color-theme-default  dark
  :enable-theme-switch  nil
  :font                 (:env EMACS_FONT)
  :font-size            (:env EMACS_FONT_SIZE)
  :modules '(core
             completion
             bindings
             git
             org
             appearance
             icons
             modeline
             dired
             evil
             elm
             eshell
             term
             search
             go
             python
             clojure
             php
             web
             work))


(provide 'init)
;;; init.el ends here
