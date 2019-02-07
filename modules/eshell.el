;;; eshell --- Summary


;;; Commentary:

;;; Code:

(defun eshell/clear ()      
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)))


(provide 'eshell)
;;; eshell.el ends here
