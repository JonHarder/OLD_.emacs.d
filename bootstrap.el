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

(setq use-package-always-defer t
      use-package-minimum-reported-time 0.001
      use-package-verbose t
      use-package-compute-statistics t)
(defvar use-package-always-defer t)

(require 'use-package)

(use-package gcmh
  :ensure t
  :demand t
  :config
  (gcmh-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :defer 1
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(defmacro defer! (time-or-feature &rest forms)
  "Execute FORMS after TIME-OR-FEATURE has occurred.

TIME-OR-FEATURE could be a number or a symbol;

If a number, it will wait the specefied number of seconds
before executing FORMS.

If a symbol, it will defer the execution of FORMS until the
feature/file has been loaded.

Examples:

waits three seconds, then evaluates the `message'
expression.

  \(defer! 3
    \(message \"I waited 3 seconds\"\)\)


Sets up a deferred evaluation of the `message' expression
until the org has been loaded.

  \(defer! 'org
    \(message \"Org mode loaded\"\)\)"
  `(cond
    ((symbolp ,time-or-feature)
     (with-eval-after-load ,time-or-feature
       ,@forms))
    ((numberp time-or-feature)
     (run-with-idle-timer ,time-or-feature
                          nil
                          (lambda ()
                             ,@forms)))))

(provide 'bootstrap)
;;; bootstrap.el ends here
