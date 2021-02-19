;;; contrib -- generic low level helper functions

;;; Commentary:
;; defines miscellaneous helper functions to operate on (surprise) lists

;;; Code:
(defun random-alnum ()
  "Generate a random alphanumeric character."
  (let* ((alnum "abcdefghijklmnopqrstuvwxyz01234567890")
         (i (% (abs (random)) (length alnum))))
    (substring alnum i (1+ i))))

(defun random-str (n)
  "Generate a random alphanumeric string of N characters."
  (interactive (list (read-number "Number of characters: " 24)))
  (dotimes (i (if (null n) 24 n))
           (insert (random-alnum))))

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
