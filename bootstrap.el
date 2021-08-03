;;; bootstrap -- Pre-configuration initialization -*- lexical-binding: t -*-

;;; Commentary:
;; initializes straight.el, loads use-package, defines the module concept.

;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "ext_lisp"))

;; settings these here because some package is loading something
;; from evil which means the variables are being read before I
;; have a chance to set them.  I don't want to dig through
;; every package I have to figure out which one it is, so I'm
;; just setting the vars here.
(defvar evil-want-keybinding nil)
(defvar evil-want-integration t)
(defvar evil-want-C-u-scroll nil)

(setq ad-redefinition-action 'accept)

;; stop emacs from slowing to a halt on long lines
(setq bidi-paragraph-direction 'left-to-right)
(if (version<= "27.1" emacs-version)
    (progn
      (setq bidi-inhibit-bpa t)
      (global-so-long-mode 1)))

(require 'package)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
;; (package-initialize)
;;; this seems to be running every time, commenting out for now
;; (unless package-archive-contents
;;   (message "refreshing packages")
;;   (package-refresh-contents))


(unless (package-installed-p 'use-package)
  (package-install 'use-package))
;; (require 'use-package)
(setq use-package-always-defer t
      use-package-minimum-reported-time 0.001
      use-package-verbose t
      use-package-compute-statistics t)
(defvar use-package-always-defer t)

(use-package exec-path-from-shell
  :ensure t
  :defer 10
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(defmacro defer! (time &rest forms)
  "Execute FORMS after Emacs has been idle for TIME secs."
  `(run-with-idle-timer ,time
                        nil
                        (lambda ()
                           ,@forms)))

(provide 'bootstrap)
;;; bootstrap.el ends here
