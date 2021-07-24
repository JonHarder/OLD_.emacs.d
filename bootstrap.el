;;; bootstrap -- Pre-configuration initialization -*- lexical-binding: t -*-

;;; Commentary:
;; initializes straight.el, loads use-package, defines the module concept.

;;; Code:

;; Bootstrap straight.el
(require 'contrib "~/.emacs.d/contrib")

(add-to-list 'load-path (concat user-emacs-directory "ext_lisp"))

;; settings these here because some package is loading something
;; from evil which means the variables are being read before I
;; have a chance to set them.  I don't want to dig through
;; every package I have to figure out which one it is, so I'm
;; just setting the vars here.
(setq evil-want-keybinding nil
      evil-want-integration t
      evil-want-C-u-scroll nil)

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

(use-package quelpa
  :ensure t)

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))

(require 'quelpa-use-package)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; some performance hacks
;; don't modify packages in place.
;; (defvar straight-check-for-modifications nil)
(defvar apropos-do-all t)
(setq after-focus-change-function #'garbage-collect)

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; core settings behavior
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

(defun config/init-with-config (config)
  "Initialize configuration using settings found in CONFIG."
  (let* ((modules (alist-get :modules config)))
      (mapc (lambda (module)
              (config/load-module module config))
            modules)))

(defmacro defconfig (config-name &rest params)
  "Contruct a settings object called CONFIG-NAME out of the PARAMS."
  `(progn
     (when (not (boundp (quote ,config-name)))
       (defvar ,config-name nil
         "The configuration object storing settings for emacs."))
     (setq ,config-name (contrib/plist-to-alist (quote ,params)))
     (config/init-with-config ,config-name)))


(provide 'bootstrap)
;;; bootstrap.el ends here
