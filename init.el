;;; init --- Summary
;; my emacs configuration

;;; Commentary:
;; bootstrap the package system, configure the module loader
;; load modules

;;; Code:
;; (package-initialize)
(require 'bootstrap "~/.emacs.d/bootstrap.el")


(defconfig jh/config
  :color-theme          spacemacs-dark
  :color-theme-package  spacemacs-themes
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
            search
            python
            clojure
            php
            web
            ansible
            work))


(provide 'init)
;;; init.el ends here
