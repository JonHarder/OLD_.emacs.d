;;; contrib -- generic low level helper functions -*- lexical-binding: t -*-

;;; Commentary:
;; defines miscellaneous helper functions to operate on (surprise) lists


;;; Code:

;;;###autoload
(defun jh/dir-files (folder extension)
  "Get all files recursively in FOLDER of the file type EXTENSION."
  (directory-files-recursively folder (rx (+ alphanumeric) "." (literal extension)) t))

;;;###autoload
(defun killport (port)
  "Kill all processes using the port number PORT."
  (interactive "nPort: ")
  (let* ((command (concat "lsof -n -i4TCP:" (number-to-string port) " | grep LISTEN | awk '{ print $2 }'"))
         (used-ports (shell-command-to-string command)))
    (shell-command (concat "kill -9 " used-ports))))


;;;###autoload
(defun find-windows-with-mode (mode)
  "Given the symbol MODE, return a list of windows where MODE is the major mode."
  (seq-filter (lambda (window)
                (with-current-buffer (window-buffer window)
                  (eq major-mode mode)))
              (window-list)))

;;;###autoload
(defun open-notes-file ()
  "Open a kotl note file for today."
  (interactive)
  (find-file (format-time-string "~/notes/%Y-%m-%d.kotl")))

;;;###autoload
(defun jh/expand-directory (dir)
  "Expand the given DIR to the list of all of its files."
  (interactive)
  (directory-files (expand-file-name dir)
                   nil
                   "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))

;;;###autoload
(defun random-alnum ()
  "Generate a random alphanumeric character."
  (let* ((alnum "abcdefghijklmnopqrstuvwxyz01234567890")
         (i (% (abs (random)) (length alnum))))
    (substring alnum i (1+ i))))

;;;###autoload
(defun random-token ()
  "Generate a random character."
  (let* ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!@#$%^&*(){}|[]")
         (i (% (abs (random)) (length chars))))
    (substring chars i (1+ i))))

;;;###autoload
(defun random-tokens (n generator)
  "Insert N tokens using the function GENERATOR."
  (interactive (list (read-number "Number of characters: " 24)
                     #'random-token))
  (dotimes (i n)
    (insert (funcall generator))))

;;;###autoload
(defun random-token-24 ()
  "Generate 24 random tokens using `random-token'."
  (interactive)
  (random-tokens 24 #'random-token))

;;;###autoload
(defun contrib/map-alist-values (f alist)
  "Map function F over each value in the ALIST.

Preserves order and keys."
  (interactive)
  (mapcar (lambda (p) (cons (car p) (funcall f (cdr p))))
          alist))


;;;###autoload
(defun contrib/plist-to-alist (plist &optional alist)
  "Convert a PLIST into an ALIST."
  (interactive)
  (let ((alist (if (null alist) '() alist)))
    (if plist
        (let* ((new-alist-front (cons (car plist) (cadr plist)))
               (new-alist (cons new-alist-front alist)))
          (contrib/plist-to-alist (cddr plist) new-alist))
      (reverse alist))))

;;;###autoload
(defun contrib/alist-keys (alist)
  "Get list of keys in the ALIST."
  (mapcar #'car alist))


;;;###autoload
(defun contrib/str-join (items glue)
  "Concatenate the list of strings, ITEMS together, separated the string GLUE."
  (mapconcat 'identity items glue))


;;;###autoload
(defun contrib/read-file-to-lines (file)
  "Read FILE into a list of strings."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n")))


;;;###autoload
(defmacro with-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;;;###autoload
(defun update-frame-font-size (size)
  "Update current frame font size to SIZE."
  (interactive (list (read-number "Font size: " jh/font-size)))
  (set-frame-font (format "%s %s" jh/font size) t t)
  (setq jh/font-size size))

(provide 'contrib)
;;; contrib.el ends here
