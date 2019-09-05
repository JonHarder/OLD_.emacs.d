;;; init --- Summary
;; my emacs configuration

;;; Commentary:
;; bootstrap the package system, configure the module loader
;; load modules

;;; Code:
;; (package-initialize)
(require 'bootstrap "~/.emacs.d/bootstrap.el")


(defconfig jh/config
  :color-theme          doom-solarized-dark
  :color-theme-package  doom-themes
  :font                 (:env EMACS_FONT)
  :font-size            15
  :modules (core
            appearance
            modeline
            completion
            bindings
            git
            org
            icons
            dired
            evil
            elm
            eshell
            term
            terraform
            search
            python
            clojure
            php
            web
            ansible
            work))


(provide 'init)
;;; init.el ends here
