;;; init --- Summary
;; my emacs configuration

;;; Commentary:
;; bootstrap the package system, configure the module loader
;; load modules

;;; Code:
;; (package-initialize)
(require 'bootstrap "~/.emacs.d/bootstrap.el")


(defconfig jh/config
  :color-theme "doom-dracula"
  :font "Fira Code"
  :font-size 17
  :modules '(core
             org
             bindings
             appearance
             modeline
             evil
             elm
             eshell
             search
             python
             clojure
             php
             web
             work))


(jh/config-init jh/config)


(provide 'init)
;;; init.el ends here
