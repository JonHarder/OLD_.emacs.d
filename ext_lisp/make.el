;;; make -- Summary:

;;; Commentary:


;;; Code:
(require 'consult)

(defun find-makefile (&optional max-directories-up)
  "Find the makefile.

Will search up to MAX-DIRECTORIES-UP before giving up."
  (setq max-directories-up (or max-directories-up 3))
  (let ((dir "./")
        (result nil))
    (while (> max-directories-up 0)
      (let ((path (concat dir "Makefile")))
        (if (file-exists-p path)
            (progn
              (setq max-directories-up 0
                    result path))
          (setq path (concat "../" path)
                max-directories-up (1- max-directories-up)))))
    result))
          

(defun make-parse-commands ()
  "Return the list of commands defined in `default-directory' Makefile."
  (with-temp-buffer
    (insert-file-contents "./Makefile")
    (cl-loop while (search-forward-regexp "^[a-zA-Z_-]+" nil t)
             collect (match-string 0))))

(defun make-run-command (command)
  "Run the specified make COMMAND."
  (compile (concat "make " command)))

(defun make ()
  "Execute make, completing the command name as a result of parsing the makefile."
  (interactive)
  (make-run-command
   (consult--read
    (make-parse-commands)
    :prompt "Command: "
    :category 'string)))

(provide 'make)
;;; make.el ends here
