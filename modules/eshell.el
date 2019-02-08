;;; eshell --- Summary


;;; Commentary:

;;; Code:

(defun eshell/clear ()
  "Clears the screen."
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)))

(provide 'eshell)
;;; eshell.el ends here
