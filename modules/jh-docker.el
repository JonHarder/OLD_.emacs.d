;;; docker --- Summary


;;; Commentary:

;;; Code:
(defun modules/docker--load (config)
  "Use CONFIG to load Dockerfile syntax and an interactive docker UI."
  (use-package dockerfile-mode
    :mode "\\.Dockerfile")
  (straight-use-package 'docker))

(provide 'jh-docker)
;;; jh-docker.el ends here
