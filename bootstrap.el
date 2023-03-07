;;; bootstrap -- Pre-configuration initialization -*- lexical-binding: t -*-

;;; Commentary:
;; initializes straight.el, loads use-package, defines the module concept.

;;; Code:

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
  :demand t
  :init
  (setq gcmhh-idle-delay 0.5
        gcmh-high-cons-threshold (* 16 1024 1024))
  :config
  (gcmh-mode 1))

;;; NOTE: esp. with fish shell, I've had the best luck
;;; by setting path directories in ~/.profile, not
;;; in the fish config itself.
;;; 2022-03-08
;;; this resolved an issue with my go environment
;;; not being configured correctly

;; (use-package exec-path-from-shell
;;   :when (memq window-system '(mac ns x))
;;   :init
;;   (setq exec-path-from-shell-arguments nil)
;;  :hook (emacs-startup . exec-path-from-shell-initialize))

(defun parse-export (line)
  "Return cons cell of (VARABLE . VALUE) as parsed from an `export VAR=VAL` LINE."
  (let* ((pieces (split-string line "="))
         (var (nth 1 (split-string (car pieces) " ")))
         (val (cadr pieces)))
    (cons var val)))

;;; load PATH
(defun parse-profile ()
  "Parse the `~/.profile' file, extracting export statements."
  (interactive)
  (with-temp-buffer
    (insert-file-contents "~/.profile")
    (let* ((contents (buffer-string))
           (lines (string-lines contents))
           (export-lines (seq-filter (lambda (line) (string-prefix-p "export" line)) lines))
           (env-vars (mapcar #'parse-export export-lines)))
      (dolist (env-var env-vars)
        (setenv (car env-var) (cdr env-var)))
      (setq exec-path (split-string (getenv "PATH") ":")))))

;;;
(defvar jh/ARCH (string-trim (shell-command-to-string "uname -m")))

(defvar jh/MAC-P (string= (window-system) "ns"))

;;; Add new homebrew path for m1 macs
(defvar MAC-M1-P
  (and jh/MAC-P
       (string= jh/ARCH "arm64")))

(when MAC-M1-P
  (add-to-list 'exec-path "/opt/homebrew/bin"))

;; (add-hook 'emacs-startup-hook 'parse-profile)
   

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
