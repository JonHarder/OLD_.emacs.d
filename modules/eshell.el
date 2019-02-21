;;; eshell --- Summary


;;; Commentary:

;;; Code:

(defun eshell/clear ()
  "Clears the screen."
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)))


(setq eshell-prompt-function
  (lambda ()
    (concat
      (eshell/pwd)
      " $ ")))


(provide 'eshell)
;;; eshell.el ends here
