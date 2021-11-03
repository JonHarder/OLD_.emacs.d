;;; osx --- Summary


;;; Commentary:


;;; Code:
(defvar *osx/alert-timer* nil "Variable which stores the timer created by `osx/alert-after`.")


(defun osx/run-script (name &optional dir)
  "Run the applescript NAME found in DIR.
Uses `user-emacs-directory'/mac if DIR is not specified."
  (let ((dir (or dir (concat user-emacs-directory "mac"))))
    (shell-command (concat "osascript " (expand-file-name name dir)))))


(defun osx/run-command (command)
  "Run the command COMMAND using osascript."
  (interactive "MCommand: ")
  (start-process
   "Notification"
   nil
   "osascript"
   "-e" command))


(defun osx/notify (message)
  "Display MESSAGE as an OSX alert window."
  (interactive "MMessage: ")
  (osx/run-command (concat "display alert \"" message "\"")))


;;;; TODO: update this to take a string for time
;;;; and parse it based on presence of ':'
;;;; using `run-at-time' instead of `run-with-timer'
;;;; TODO: update parsing of time for relative
;;;; time ammounts e.x. 5m for "5 minutes", 5h for "5 hours"
(defun osx/alert-after (time &optional message)
  "Alert after TIME minutes with optional MESSAGE."
  (interactive "nAlert after (minutes): \nMMessage: ")
  (setq *osx/alert-timer*
        (run-with-timer (* 60 time) nil
                  (lambda ()
                    (osx/notify (or message "Alert"))))))



(provide 'osx)
;;; osx.el ends here
