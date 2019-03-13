;;; init --- Summary
;; my emacs configuration

;;; Commentary:
;; bootstrap the package system, configure the module loader
;; load modules

;;; Code:
;; (package-initialize)
(require 'bootstrap "~/.emacs.d/bootstrap.el")

;; doom-Iosvkem is good
;; doom-solarized-light
;; doom-tomorrow-night
;; spacemacs-dark
(defconfig jh/config
  :color-theme-light spacemacs-light
  :color-theme-dark spacemacs-dark
  :font "Fira Code"
  :font-size 17
  :modules '(core
             completion
             bindings
             jorg
             appearance
             icons
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
