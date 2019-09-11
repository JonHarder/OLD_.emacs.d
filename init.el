;;; init --- Summary
;; my emacs configuration

;;; Commentary:
;; bootstrap the package system, configure the module loader
;; load modules

;;; Code:
;; (package-initialize)
(require 'bootstrap "~/.emacs.d/bootstrap.el")

(defconfig jh/config
  :color-theme          (:env EMACS_COLOR_THEME)
  :color-theme-package  doom-themes
  :font                 (:env EMACS_FONT)
  :font-size            (:env EMACS_FONT_SIZE)
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
            lisp
            elm
            eshell
            term
            terraform
            search
            python
            rss
            clojure
            php
            web
            ansible
            work))


(provide 'init)
;;; init.el ends here
