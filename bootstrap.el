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


(use-package exec-path-from-shell
 :straight t
 :config
 (when (memq window-system '(mac ns x))
   (setq exec-path-from-shell-variables
         '("PATH"
           "MANPATH"
           "ANSIBLE_PLAYBOOK_DIR"
           "EMACS_FONT"
           "EMACS_FONT_SIZE"))
   (exec-path-from-shell-initialize)))


(defun load-module (module-name &optional module-path)
  "Load the module called MODULE-NAME defined in my modules folder, or in MODULE-PATH if given."
  (let* ((module-path (or module-path
                          (concat user-emacs-directory "modules/")))
         (path (concat module-path "jh-" (symbol-name module-name) ".el")))
    (load path)))


(defun config/is-env (s)
  "Determine if the configuration value S is an environment variable."
  (interactive)
  (and (listp s) (eq (car s) :env)))


(defun config/get-env (s &optional is-bool)
  "Get the value of the environment variable S, normalizing to bool if IS-BOOL."
  (interactive)
  (let ((var (getenv (symbol-name s))))
    (if is-bool
        (string-equal "1" var)
      var)))


(defun config/eval-var (var)
  "Get the value of the from object CONFIG under the setting CONFIG-KEY.

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
  (let ((font (alist-get :font config))
        (font-size (alist-get :font-size config))
        (modules (alist-get :modules config)))
    (mapc 'load-module modules)
    (set-frame-font (format "%s %s" font font-size))
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
