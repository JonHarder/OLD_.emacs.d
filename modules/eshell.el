;;; eshell --- Summary


;;; Commentary:

;;; Code:
(require 'dash)
(use-package dash-functional
  :ensure t)
(require 's)

(setq eshell-banner-message "")

(defvar pretty-eshell-funcs nil
  "List of `pretty-eshell-section' to enable.")

(defvar pretty-eshell-sep " | "
  "String delimits each `pretty-eshell-section'")

(defvar pretty-eshell-section-delim " "
  "String delimits icons and their text.")

(defvar pretty-eshell-header ""
  "Initial string composing the eshell prompt.")

(defvar pretty-eshell-prompt-string " "
  "Prompt string, must match builtin `eshell-prompt-regexp'")

(setq eshell-prompt-regexp " ")

(defvar pretty-eshell-prompt-num 0
  "Prompt number for current eshell session.")

;;; Core

(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))

(defmacro pretty-eshell-section (name icon form &rest props)
  "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
  ;; Roundabout way to handle case that
  ;; 1. Form is a variable and
  ;; 2. That variable might not be defined/initialized
  ;; Eg. pyvenv-virtualenv-name not loaded until pyvenv-workon
  `(setq ,name
         (lambda ()
           (when (or (and (symbolp (quote ,form))
                          (bound-and-true-p ,form))
                     (and (not (symbolp (quote ,form)))
                          ,form))
             (-> ,icon
                (concat pretty-eshell-section-delim ,form)
                (with-face ,@props))))))

(defun pretty-eshell--acc (acc x)
  "Accumulator for evaluating and concatenating pretty-eshell-sections."
  (--if-let (funcall x)
      (if (s-blank? acc)
          it
        (s-concat acc pretty-eshell-sep it))
    acc))

(defun pretty-eshell-prompt-func ()
  "Value for `eshell-prompt-function'."
  (concat pretty-eshell-header
          (-reduce-from 'pretty-eshell--acc "" pretty-eshell-funcs)
          "\n"
          pretty-eshell-prompt-string))

;;; define the sections

(pretty-eshell-section
 esh-dir
 "\xf016"  ;
 (abbreviate-file-name (eshell/pwd))
 '(:foreground "#268bd2" :bold bold :underline t))

;; Git Branch
(pretty-eshell-section
 esh-git
 "\xe907"  ; 
 (magit-get-current-branch)
 '(:foreground "#8D6B94"))

;; Python Virtual Environment
(pretty-eshell-section
 esh-python
 "\xe928"  ; 
 pyvenv-virtual-env-name)

;; Time
(pretty-eshell-section
 esh-clock
 "\xf017"  ; 
 (format-time-string "%I:%M %p" (current-time))
 '(:foreground "forest green"))

(setq pretty-eshell-funcs
      (list esh-dir esh-git esh-python esh-clock))

(setq eshell-prompt-function
      'pretty-eshell-prompt-func)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; custom functions

(defun eshell/clear ()
  "Clears the screen."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/c ()
  "A shorthand alias for eshell/clear."
  (interactive)
  (eshell/clear))


(defun eshell/e (file)
  "Shorthand command to open FILE."
  (find-file file))


(add-hook 'eshell-mode-hook
  (lambda ()
    (define-key eshell-mode-map
      (kbd "<tab>")
      (lambda ()
        (interactive)
        (completion-at-point)))))


(provide 'eshell)
;;; eshell.el ends here
