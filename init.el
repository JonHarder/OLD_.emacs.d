;;; init --- Summary

;;; Commentary:
;; my Emacs configuration

;;; Code:

;;;; load dependencies
(load (expand-file-name "contrib" user-emacs-directory))
(load (expand-file-name "bootstrap" user-emacs-directory))

;;;; Define basic display configuration variables
(defgroup configuration nil
  "Group for personal configuration."
  :group 'emacs)

(defcustom jh/font-size 13
  "Default font size all buffers will display as."
  :type 'integer
  :group 'configuration)

(defcustom jh/font "Source Code Pro"
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

(defcustom jh/scale-org-headings t
  "Whether or not to scale org headings."
  :type 'boolean
  :group 'configuration)

;;;; Overrides to defaults
(setq jh/font-size 13)
(setq jh/theme "kaolin")

;;;; Load configuration proper
(mapc #'load-file (directory-files "~/.emacs.d/modules" t ".+\.el"))

(provide 'init)
;;; init.el ends here
