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

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)


(defun load-module (module-name &optional module-path)
  "Load the module called MODULE-NAME defined in my modules folder, or in MODULE-PATH if given."
  (let* ((module-path (or module-path
                          (concat user-emacs-directory "modules/")))
         (path (concat module-path (symbol-name module-name) ".el")))
    (load path)))


(defun defmodules (modules)
  "Load the given MODULES."
  (mapc 'load-module modules))


(defun jh/config-init (config)
  "Initialize configuration using settings found in CONFIG."
  (let ((color-theme (plist-get config :color-theme))
        (font (plist-get config :font))
        (font-size (plist-get config :font-size))
        (modules (plist-get config :modules)))
    (defmodules (cadr modules))
    (set-frame-font (format "%s %i" font font-size))))


(defmacro defconfig (config-name &rest params-plist)
  "Contruct a settings object called CONFIG-NAME out of the PARAMS-PLIST."
  `(setq ,config-name
     (quote ,params-plist)))


(provide 'bootstrap)
;;; bootstrap.el ends here
