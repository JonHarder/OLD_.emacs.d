;;; -*- lexical-binding: t -*-
(defun modules/docker--load (config)
  "Use CONFIG to load Dockerfile syntax and an interactive docker UI."

  (use-package dockerfile-mode
    :ensure t
    :mode "\\.Dockerfile")

  (use-package docker-compose-mode
    :ensure t
    :mode "docker-compose.yml")

  (use-package docker
    :ensure t
    :commands (docker-images docker-containers docker-networks docker-volumes)
    :config
    (dolist (mode '(docker-image-mode
                    docker-container-mode
                    docker-network-mode
                    docker-volume-mode))
      (add-to-list 'evil-emacs-state-modes mode))))
