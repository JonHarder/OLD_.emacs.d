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


(defun load-module (module-name &optional module-path)
  "Load the module called MODULE-NAME defined in my modules folder, or in MODULE-PATH if given."
  (let* ((module-path (or module-path
                          (concat user-emacs-directory "modules/")))
         (path (concat module-path "jh-" (symbol-name module-name) ".el")))
    (load path)))


(defun config/is-env (s)
  (interactive)
  (and (listp s) (eq (car s) :env)))


(defun config/get-env (s &optional is-bool)
  (interactive)
  (let ((var (getenv (symbol-name s))))
    (if is-bool
        (eq "1" var)
      var)))

(defun config/eval-var (var-symbol)
  "Get an environment variable based of the symbol VAR-SYMBOL, normalizing to t/nil if IS-BOOL is true."
  (interactive)
  (if (config/is-env var-symbol)
      (config/get-env (cadr var-symbol) (caddr var-symbol))
    var-symbol))


(defun jh/config-init (config)
  "Initialize configuration using settings found in CONFIG."
  (let ((font (config/eval-var (plist-get config :font)))
        (font-size (config/eval-var (plist-get config :font-size)))
        (modules (plist-get config :modules)))
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
