;;; init --- Summary
;; my emacs configuration

;;; Code:
(load (concat user-emacs-directory "bootstrap"))


(defvar jh/font "Source Code Pro")
(defvar jh/font-size 14)
(defvar jh/theme "modus")
(defvar jh/highlight-line nil)
(defvar jh/scale-org-headings t)


(defvar config-modules-directory
  (concat user-emacs-directory "modules"))

(defconst config-module-generated-file
  (concat user-emacs-directory "modules-computed.el"))

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
  (file-attribute-modification-time (file-attributes config-module-generated-file)))

(defun regenerate-config-p ()
  "Predicate which indicates if the configuration regenrate the combined modules file."
  (interactive)
  (let ((module-file-mod-time (module-generated-file-modified-time)))
    (if (null module-file-mod-time)
        t
      (time-less-p module-file-mod-time
                   (compute-last-modified-time)))))

(defun regenerate-config (generate-file-name)
  "Foo bar GENERATE-FILE-NAME."
  (message "Regenerating computed module file: %s" config-module-generated-file)
  (with-temp-file generate-file-name
    (mapc (lambda (f)
            (insert (with-temp-buffer
                      (insert-file-contents f)
                      (buffer-string))
                    "\n"))
          (seq-drop (directory-files config-modules-directory t) 2)))
  (byte-compile-file config-module-generated-file))

(when (regenerate-config-p)
    (regenerate-config config-module-generated-file))
(load-file (byte-compile-dest-file config-module-generated-file))

(add-hook 'after-init-hook
          (lambda ()
           (message "Initilization completed in %s" (emacs-init-time))))

(provide 'init)
;;; init.el ends here
