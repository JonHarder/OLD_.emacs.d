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
 :if (memq window-system '(mac ns))
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


(defun config/eval-var (config config-key)
  "Get the value of the from object CONFIG under the setting CONFIG-KEY.

This is aware of the different shapes a configuration value can take,
including hardcoded values, and values stored in environment variables
e.x. (:env FOO_BAR)"
  (interactive)
  (let ((var-symbol (plist-get config config-key)))
   (if (config/is-env var-symbol)
       (config/get-env (cadr var-symbol) (caddr var-symbol))
     var-symbol)))


(defun jh/config-init (config)
  "Initialize configuration using settings found in CONFIG."
  (let ((font (config/eval-var config :font))
        (font-size (config/eval-var config :font-size))
        (modules (config/eval-var config :modules)))
    (mapc 'load-module (cadr modules))
    (set-frame-font (format "%s %s" font font-size))))


(defmacro defconfig (config-name &rest params-plist)
  "Contruct a settings object called CONFIG-NAME out of the PARAMS-PLIST."
  `(progn
     (setq ,config-name
           (quote ,params-plist))
     (jh/config-init ,config-name)))


(provide 'bootstrap)
;;; bootstrap.el ends here
