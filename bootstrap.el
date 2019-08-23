;;; bootstrap -- Pre-configuration initialization

;;; Commentary:
;; initializes package.el, loads use-package, defines the module concept.

;;; Code:

;; Bootstrap straight.el
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
;; End bootstrap straight.el

(defvar config/env-vars
  '("ANSIBLE_PLAYBOOK_DIR"
    "EMACS_FONT"
    "EMACS_FONT_SIZE"))


(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-variables
          (append '("PATH" "MANPATH")
                  config/env-vars))
    (exec-path-from-shell-initialize)))


(defun module-init-func (module-name)
  "Helper function to get the load method of MODULE-NAME."
  (intern (format "modules/%s--load" module-name)))


(defun jh/load-module (module config &optional module-path)
  "Load the MODULE using given CONFIG, loading from MODULE-PATH if provided."
  (let* ((module-path (or module-path
                          (concat user-emacs-directory "modules/")))
         (module-name (symbol-name module))
         (path (concat module-path "jh-" module-name ".el"))
         (load-func (module-init-func module-name)))
    (load path)
    (apply load-func (list config))))


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
      var)))


(defun config/eval-var (var)
  "Evaluate VAR to see if it is a regular value, or a special form.

This is aware of the different shapes a configuration value can take,
including hardcoded values, and values stored in environment variables
e.x. (:env FOO_BAR)"
  (interactive)
   (if (config/is-env var)
       (config/get-env (cadr var) (caddr var))
     var))


(defun jh/load-config (config)
  "Evaluate any special forms in CONFIG."
  (map-alist-values #'config/eval-var config))


(defun jh/config-init (config)
  "Initialize configuration using settings found in CONFIG."
  (let ((modules (alist-get :modules config)))
    (mapc (lambda (module) (jh/load-module module config)) modules)
    config))


(defun plist-to-alist (plist &optional alist)
  "Convert a PLIST into an ALIST."
  (interactive)
  (let ((alist (if (null alist) '() alist)))
    (if plist
        (let* ((new-alist-front (cons (car plist) (cadr plist)))
               (new-alist (cons new-alist-front alist)))
          (plist-to-alist (cddr plist) new-alist))
      (reverse alist))))


(defun map-alist-values (f alist)
  "Map function F over each value in the ALIST.

Perserves order and keys."
  (interactive)
  (mapcar (lambda (p) (cons (car p) (funcall f (cdr p))))
          alist))


(defmacro defconfig (config-name &rest params-plist)
  "Contruct a settings object called CONFIG-NAME out of the PARAMS-PLIST."
  `(progn
     (when (not (boundp (quote ,config-name)))
       (defvar ,config-name nil
         "The configuration object storing settings for emacs."))
     (setq ,config-name
           (jh/load-config (plist-to-alist (quote ,params-plist))))
     (jh/config-init ,config-name)))


(provide 'bootstrap)
;;; bootstrap.el ends here
