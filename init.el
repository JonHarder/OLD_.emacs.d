;;; init --- Summary
;; my emacs configuration

;;; Commentary:
;; bootstrap the package system, configure the module loader
;; load modules

;;; Code:
;; (package-initialize)

(message "bootstrappin system")
(require 'bootstrap "~/.emacs.d/bootstrap.el")

(message "loading modules")

(defmodules
  core
  org
  appearance
  evil
  search
  php)

(message "finished loading modules")


(find-file "~/Org/todo.org")

(provide 'init)
;;; init.el ends here
