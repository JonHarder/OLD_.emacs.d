;;; bootstrap -- Pre-configuration initialization -*- lexical-binding: t -*-

;;; Commentary:
;; initializes straight.el, loads use-package, defines the module concept.

;;; Code:

;; Bootstrap straight.el
(require 'contrib "~/.emacs.d/contrib")

;; settings these here because some package is loading something
;; from evil which means the variables are being read before I
;; have a chance to set them.  I don't want to dig through
;; every package I have to figure out which one it is, so I'm
;; just setting the vars here.
(setq evil-want-keybinding nil
      evil-want-integration t
      evil-want-C-u-scroll nil)

;; stop emacs from slowing to a halt on long lines
(setq bidi-paragraph-direction 'left-to-right)
(if (version<= "27.1" emacs-version)
    (progn
      (setq bidi-inhibit-bpa t)
      (global-so-long-mode 1)))

(require 'package)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)


;; some performance hacks
;; don't modify packages in place.
;; (defvar straight-check-for-modifications nil)
(defvar apropos-do-all t)
(setq frame-inhibit-implied-resize t)
(setq fast-but-imprecise-scrolling t)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq-default bidi-display-reordering 'left-to-right)
(setq idle-update-delay 1)
(setq after-focus-change-function #'garbage-collect)

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; core settings behavior
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq dired-listing-switches "-alh")
(setq ring-bell-function 'ignore)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling)
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(when (eq system-type 'darwin)
  (setq-default
   mac-option-modifier nil
   mac-command-modifier 'meta
   mac-redisplay-dont-reset-vscroll t
   mac-mouse-wheel-smooth-scroll nil))


(require 'server)
(when (not (server-running-p))
  (server-start))


(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (setq-default exec-path-from-shell-variables
                  '("PATH" "MANPATH" "GOPATH"))
    (exec-path-from-shell-initialize)))

(defun module-init-func (module-name)
  "Helper function to get the load method of MODULE-NAME."
  (intern (format "modules/%s--load" module-name)))


(defun config/load-module (module config &optional module-path)
  "Load the MODULE using given CONFIG, loading from MODULE-PATH if provided."
  (when (not (listp module)) ;; why is this here?
    (let* ((module-path (or module-path
                            (concat user-emacs-directory "modules/")))
           (module-name (symbol-name module))
           (path (concat module-path module-name))
           (load-func (module-init-func module-name)))
      (load path)
      (apply load-func (list config)))))


(defun config/is-env (s)
  "Determine if the configuration value S is an environment variable."
  (interactive)
  (and (listp s) (eq (car s) :env)))


(defun jh/getenv (name)
  "OS aware version of `getenv' to retrieve environment variable NAME."
  (require 'exec-path-from-shell)
  (if (memq window-system '(mac ns x))
     (exec-path-from-shell-getenv name)
    (getenv name)))

(defun config/get-env (s)
  "Get the value of the environment variable S, normalizing to bool if keyword :type is bool."
  (interactive)
  (let* ((var-name (plist-get s :env))
         (default  (plist-get s :default))
         (type     (plist-get s :type))
         (var (if (string-equal window-system "w32")
                  default
                (or (jh/getenv var-name)
                    default))))
    (when (null var)
      (error (format "environment variable '%s' was not found and no :default provided" var-name)))
    (if (and type (= type 'bool))
        (or (string-equal "1" var) (string-equal "true" var))
      (intern var))))


(defun config/eval-var (var)
  "Evaluate VAR to see if it is a regular value, or a special form.

This is aware of the different shapes a configuration value can take,
including hardcoded values, and values stored in environment variables.

Currently, values can be a symbol
e.x.
  (config/eval-var 'foo)

Or an environment variable
e.x.
  (config/eval-var '(:env FOO))

Environment variable names can also specify a fallback by giving a value after
the :default keyword
e.x.
  (config/eval-var '(:env FOO :default 1))"
  (interactive)
  (if (config/is-env var)
      (config/get-env var)
    var))


(defun config/load-config (config)
  "Evaluate any special forms in CONFIG."
  (contrib/map-alist-values #'config/eval-var config))


(defmacro with-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defun config/init-with-config (config)
  "Initialize configuration using settings found in CONFIG."
  (let ((modules (alist-get :modules config))
        (module-profile '())
        (prof-buf (generate-new-buffer "*profiler*")))
      (mapc (lambda (module)
                (let ((time (with-time (config/load-module module config))))
                   (setq module-profile (cons (cons module time) module-profile))))
            modules)
      (when (alist-get :profile config)
        (with-current-buffer prof-buf
          (goto-char (point-max))
          (mapc (lambda (d)
                  (insert (format "Module %s\ttook %.2f seconds\n" (car d) (cdr d))))
                (sort module-profile (lambda (x y) (> (cdr x) (cdr y)))))))
      (display-buffer prof-buf)))


(defmacro defconfig (config-name &rest params)
  "Contruct a settings object called CONFIG-NAME out of the PARAMS."
  `(progn
     (when (not (boundp (quote ,config-name)))
       (defvar ,config-name nil
         "The configuration object storing settings for emacs."))
     (setq ,config-name
           (config/load-config (contrib/plist-to-alist (quote ,params))))
     (config/init-with-config ,config-name)))


(provide 'bootstrap)
;;; bootstrap.el ends here
