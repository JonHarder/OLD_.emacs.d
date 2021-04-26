;;; contrib -- generic low level helper functions

;;; Commentary:
;; defines miscellaneous helper functions to operate on (surprise) lists

;;; Code:
(defun find-windows-with-mode (mode)
  "Given the symbol MODE, return a list of windows where MODE is the major mode."
  (seq-filter (lambda (window)
                (with-current-buffer (window-buffer window)
                  (eq major-mode mode)))
              (window-list)))


(defun jh/smart-insert-brace ()
  "Insert an opening and closing brace, indenting properly according to the mode."
  (interactive)
  (insert "{\n\n}")
  (indent-for-tab-command)
  (forward-line -1)
  (indent-for-tab-command))

(defun jh/expand-directory (dir)
  "Expand the given DIR to the list of all of its files."
  (interactive)
  (directory-files (expand-file-name dir)
                   nil
                   "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))

(defun random-alnum ()
  "Generate a random alphanumeric character."
  (let* ((alnum "abcdefghijklmnopqrstuvwxyz01234567890")
         (i (% (abs (random)) (length alnum))))
    (substring alnum i (1+ i))))

(defun random-token ()
  "Generate a random character."
  (let* ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!@#$%^&*(){}|[]")
         (i (% (abs (random)) (length chars))))
    (substring chars i (1+ i))))

(defun random-tokens (n generator)
  "Insert N tokens using the function GENERATOR."
  (interactive (list (read-number "Number of characters: " 24)
                     #'random-token))
  (dotimes (i n)
    (insert (funcall generator))))

(defun random-token-24 ()
  "Generate 24 random tokens using `random-token'."
  (interactive)
  (random-tokens 24 #'random-token))

(defun contrib/map-alist-values (f alist)
  "Map function F over each value in the ALIST.

Preserves order and keys."
  (interactive)
  (mapcar (lambda (p) (cons (car p) (funcall f (cdr p))))
          alist))


(defun contrib/plist-to-alist (plist &optional alist)
  "Convert a PLIST into an ALIST."
  (interactive)
  (let ((alist (if (null alist) '() alist)))
    (if plist
        (let* ((new-alist-front (cons (car plist) (cadr plist)))
               (new-alist (cons new-alist-front alist)))
          (contrib/plist-to-alist (cddr plist) new-alist))
      (reverse alist))))

(defun contrib/alist-keys (alist)
  "Get list of keys in the ALIST."
  (mapcar #'car alist))


(defun contrib/str-join (items glue)
  "Concatenate the list of strings, ITEMS together, separated the string GLUE."
  (mapconcat 'identity items glue))


(defun contrib/read-file-to-lines (file)
  "Read FILE into a list of strings."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n")))

(provide 'contrib)
;;; contrib.el ends here
