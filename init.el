;;; init --- Summary

;;; Commentary:
;; my Emacs configuration

;;; Code:
(load (expand-file-name "contrib" user-emacs-directory))
(load (expand-file-name "bootstrap" user-emacs-directory))

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

(defcustom jh/theme "doom"
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

(dolist (module (jh/dir-files "~/.emacs.d/modules" "el"))
  (load-file module))


(provide 'init)
;;; init.el ends here
