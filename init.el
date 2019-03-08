;;; init --- Summary
;; my emacs configuration

;;; Commentary:
;; bootstrap the package system, configure the module loader
;; load modules

;;; Code:
;; (package-initialize)
(require 'bootstrap "~/.emacs.d/bootstrap.el")

;; doom-Ioskvem is good
;; doom-solarized-light
;; doom-tomorrow-night
(defconfig jh/config
  :color-theme "doom-spacegrey"
  :font "Fira Code"
  :font-size 17
  :modules '(core
             bindings
             jorg ;; naming something "org" seems to conflict with built in org
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
