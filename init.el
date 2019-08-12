;;; init --- Summary
;; my emacs configuration

;;; Commentary:
;; bootstrap the package system, configure the module loader
;; load modules

;;; Code:
;; (package-initialize)
(require 'bootstrap "~/.emacs.d/bootstrap.el")


(straight-use-package 'doom-themes)


(defconfig jh/config
  :color-theme          doom-gruvbox
  :font                 (:env EMACS_FONT)
  :font-size            (:env EMACS_FONT_SIZE)
  :modules (core
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
            ansible
            work))


(provide 'init)
;;; init.el ends here
