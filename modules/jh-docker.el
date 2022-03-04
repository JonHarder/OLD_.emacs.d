;;; docker --- summary
;;; Configuring utilities and file mode associations
;;; for the docker container tool

;;; Commentary:

;;; -*- lexical-binding: t -*-

;;; Code:
(use-package dockerfile-mode
  :mode "\\.Dockerfile")

(use-package docker-compose-mode
  :mode "docker-compose.yml")

(use-package docker
  :after (evil general)
  :commands (docker docker-images docker-containers docker-networks docker-volumes)
  :config
  (dolist (mode '(docker-image-mode
                  docker-container-mode
                  docker-network-mode
                  docker-volume-mode))
    (add-to-list 'evil-emacs-state-modes mode))
  :general
  (:keymaps 'docker-container-mode-map
   :states 'normal
   "j" 'tablist-next-line
   "k" 'tablist-previous-line
   "x" 'docker-container-kill
   "i" 'docker-container-inspect
   "l" 'docker-container-logs)
  (:keymaps 'docker-image-mode-map
   :states 'normal
   "d" #'docker-image-rm))

(provide 'jh-docker)
;;; jh-docker.el ends here
