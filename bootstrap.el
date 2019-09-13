;;; bootstrap -- Pre-configuration initialization

;;; Commentary:
;; initializes straight.el, loads use-package, defines the module concept.

;;; Code:

;; Bootstrap straight.el
(require 'contrib "~/.emacs.d/contrib")


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (boostrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq-default straight-use-package-by-default t)
;; End bootstrap straight.el

(defvar config/env-vars
  '("ANSIBLE_PLAYBOOK_DIR"
    "EMACS_FONT"
    "EMACS_FONT_SIZE"
    "EMACS_COLOR_THEME"
    "EMACS_COLOR_THEME_PACKAGE"))


(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (setq-default exec-path-from-shell-variables
          (append '("PATH" "MANPATH")
                  config/env-vars))
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
           (path (concat module-path "jh-" module-name))
           (load-func (module-init-func module-name)))
      (load path)
      (apply load-func (list config)))))


(defun config/is-env (s)
  "Determine if the configuration value S is an environment variable."
  (interactive)
  (and (listp s) (eq (car s) :env)))


(defun config/get-env (s &optional is-bool)
  "Get the value of the environment variable S, normalizing to bool if IS-BOOL."
  (interactive)
  (let ((var (getenv (symbol-name s))))
    (when (null var)
      (error (format "environment variable '%s' was not found" s)))
    (if is-bool
        (string-equal "1" var)
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
  (config/eval-var '(:env FOO))"
  (interactive)
   (if (config/is-env var)
       (config/get-env (cadr var) (caddr var))
     var))


(defun config/load-config (config)
  "Evaluate any special forms in CONFIG."
  (contrib/map-alist-values #'config/eval-var config))


(defun config/init-with-config (config)
  "Initialize configuration using settings found in CONFIG."
  (let ((modules (alist-get :modules config)))
    (mapc (lambda (module) (config/load-module module config)) modules)
    config))


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
