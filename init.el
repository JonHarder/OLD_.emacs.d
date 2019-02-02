;;; init --- Summary
;; my emacs configuration

;;; Commentary:
;; bootstrap the package system, configure the module loader
;; load modules

;;; Code:
;; (package-initialize)

(require 'bootstrap "~/.emacs.d/bootstrap.el")

(message "loading modules")

(defmodules
  core
  org
  appearance
  evil
  search
  php)

(provide 'init)
;;; init.el ends here
