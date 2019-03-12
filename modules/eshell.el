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

(defun eshell/e (file)
  "Shorthand command to open FILE."
  (find-file file))


(provide 'eshell)
;;; eshell.el ends here
