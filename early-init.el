(message "early init starting")

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(defun jh/post-init-hook ()
  (setq gc-cons-threshold (* 2 1000 1000)
        gc-cons-percentage 0.1))

(add-hook 'after-init-hook #'jh/post-init-hook)
