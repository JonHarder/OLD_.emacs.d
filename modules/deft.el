;;; -*- lexical-binding: t -*-
(defun modules/deft--load (config)
  (use-package deft
    :ensure t
    :config
    (setq deft-extensions '("org")
          deft-directory "~/zettelkasten")))
