;;; bootstrap -- Pre-configuration initialization

;;; Commentary:
;; initializes package.el, loads use-package, defines the module concept.

;;; Code:
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(defun load-module (module-name &optional module-path)
  "Load the module called MODULE-NAME defined in my modules folder, or in MODULE-PATH if given."
  (let* ((module-path (or module-path
                          (concat user-emacs-directory "modules/")))
         (path (concat module-path (symbol-name module-name) ".el")))
    (load path)))
(message "defined load-module")

(defmacro defmodules (&rest modules)
  "Load the given MODULES."
  `(mapc 'load-module (quote ,modules)))
(message "defined defmodules")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(provide 'bootstrap)
;;; bootstrap.el ends here
