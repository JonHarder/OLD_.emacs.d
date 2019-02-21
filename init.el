;;; init --- Summary
;; my emacs configuration

;;; Commentary:
;; bootstrap the package system, configure the module loader
;; load modules

;;; Code:
;; (package-initialize)
(require 'bootstrap "~/.emacs.d/bootstrap.el")

(defgroup jh nil
  "Group for storing generic customization for me."
  :group 'convenience)

(defcustom jh/color-theme "material-light"
  "The color theme to use."
  :group 'jh)

(defcustom jh/font "Fira Code"
  "The font to use for all text."
  :group 'jh)
(defcustom jh/font-size 17
  "The size of font to use."
  :group 'jh)


(defmodules
  core
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
  work)

(provide 'init)
;;; init.el ends here
