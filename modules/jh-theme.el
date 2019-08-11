;;; appearance --- Summary

;;; Commentary:

;;; Code:
(require 'seq)


(defun jh/set-theme (theme)
  "Enable THEME, disabling all others."
  (interactive)
  (let ((other-themes (seq-filter
                       (lambda (other-theme)
                         (not (string-equal theme other-theme)))
                       custom-enabled-themes)))
    (mapc 'disable-theme other-themes)
    (load-theme theme t)))


(jh/set-theme (alist-get :color-theme jh/config))


(provide 'jh-theme)
;;; jh-theme.el ends here
