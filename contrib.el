;;; contrib -- generic low level helper functions

;;; Commentary:
;; defines misselanious helper functions to operate on (surprise) lists

;;; Code:
(defun contrib/map-alist-values (f alist)
  "Map function F over each value in the ALIST.

Perserves order and keys."
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


(provide 'contrib)
;;; contrib.el ends here
