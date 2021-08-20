;;; -*- lexical-binding: t -*-
(use-package dockerfile-mode
  :mode "\\.Dockerfile")

(use-package docker-compose-mode
  :mode "docker-compose.yml")

(use-package docker
  :commands (docker docker-images docker-containers docker-networks docker-volumes)
  :config
  (dolist (mode '(docker-image-mode
                  docker-container-mode
                  docker-network-mode
                  docker-volume-mode))
    (add-to-list 'evil-emacs-state-modes mode))
  :general
  (:keymaps 'docker-container-mode-map
   :state 'normal
   "j" 'tablist-next-line
   "k" 'tablist-previous-line
   "x" 'docker-container-kill
   "i" 'docker-container-inspect
   "l" 'docker-container-logs))
