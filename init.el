;;; init --- Summary

;;; Commentary:
;; my Emacs configuration

;;; Code:
(load (concat user-emacs-directory "bootstrap"))

(defgroup configuration nil
  "Group for personal configuration."
  :group 'emacs)

(defcustom jh/font-size 14
  "Default font size all buffers will display as."
  :type 'integer
  :group 'configuration)

(defcustom jh/font "Source Code Pro"
  "Default font to use for text."
  :type 'string
  :group 'configuration)

(defcustom jh/theme "oceanic"
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

(defcustom config-modules-directory (concat user-emacs-directory "modules")
  "Directory containing configuration modules."
  :type 'directory
  :group 'configuration)

(defconst config-module-generated-file
  (concat user-emacs-directory "modules-computed"))

(defun compute-last-modified-time ()
  "Return time of the most recently edited configuration file."
  (let* ((config-files (seq-drop (directory-files config-modules-directory t) 2))
         (modified-times (mapcar (lambda (f)
                                   (file-attribute-modification-time (file-attributes f)))
                                 config-files)))
    (seq-reduce (lambda (last-modified-time f-mod-time)
                  (if (time-less-p last-modified-time f-mod-time)
                      f-mod-time
                    last-modified-time))
                modified-times
                (car modified-times))))

(defun module-generated-file-modified-time ()
  "Get modification time of `config-module-generated-file'."
  (file-attribute-modification-time (file-attributes (byte-compile-dest-file config-module-generated-file))))

(defun regenerate-config-p ()
  "Predicate which indicates if the configuration regenrate the combined modules file."
  (interactive)
  (let ((module-file-mod-time (module-generated-file-modified-time)))
    (if (null module-file-mod-time)
        t
      (time-less-p module-file-mod-time
                   (compute-last-modified-time)))))

(defun regenerate-config ()
  "Regenerate composite file `config-module-generated-file' and defer byte-compiling till Emacs is idle."
  (message "regenerating config")
  (with-temp-file (concat config-module-generated-file ".el")
    (mapc (lambda (f)
            (insert (with-temp-buffer
                      (insert-file-contents f)
                      (buffer-string))
                    "\n"))
          (seq-drop (directory-files config-modules-directory t) 2)))
  (byte-compile-file (concat config-module-generated-file ".el")))

(when (regenerate-config-p)
  (regenerate-config))

(load-file (concat config-module-generated-file ".elc"))


(provide 'init)
;;; init.el ends here
