;;; -*- lexical-binding: t -*-
(use-package dockerfile-mode
  :mode "\\.Dockerfile")

(use-package docker-compose-mode
  :mode "docker-compose.yml")

(use-package docker
  :commands (docker-images docker-containers docker-networks docker-volumes)
  :config
  (dolist (mode '(docker-image-mode
                  docker-container-mode
                  docker-network-mode
                  docker-volume-mode))
    (add-to-list 'evil-emacs-state-modes mode)))
