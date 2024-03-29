;;; init --- Summary

;;; Commentary:
;; my Emacs configuration

;;; Code:
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time))))))
(require 'use-package)
(use-package general
  :demand t)


;;;; load dependencies
(load (expand-file-name "contrib" user-emacs-directory))
(load (expand-file-name "bootstrap" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

;;;; Define basic display configuration variables
(defgroup configuration nil
  "Group for personal configuration."
  :group 'emacs)

(defcustom jh/font-size 18
  "Default font size all buffers will display as."
  :type 'integer
  :group 'configuration)

(defcustom jh/font "Iosevka"
  "Default font to use for text."
  :type 'string
  :group 'configuration)

(defcustom jh/theme (getenv "EMACS_THEME")
  "Theme to use, must be a key memeber of `jh/themes'."
  :type 'string
  :group 'configuration)

(defcustom jh/highlight-line nil
  "Whether or not to highlight the line the point is on."
  :type 'boolean
  :group 'configuration)

(defcustom jh/scale-org-headings nil
  "Whether or not to scale org headings."
  :type 'boolean
  :group 'configuration)

;;;; Overrides to defaults
(setq jh/theme "doom")
(setq jh/font-size 20)

;;;; Load configuration proper
(load-modules)

(put 'narrow-to-page 'disabled nil)

(provide 'init)
;;; init.el ends here
