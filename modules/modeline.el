;;; package --- Summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package doom-modeline
  :ensure t
  :demand t
  :config
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-hud t)
  (doom-modeline-buffer-file-name 'relative-from-project)
  (doom-modeline-enable-word-count t))

(display-time-mode t)

(provide 'modeline)
;;; modeline.el ends here
