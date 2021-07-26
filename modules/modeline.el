;;; package --- Summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun modules/modeline--load (config)
  "Load modeline configuration using CONFIG."
  (use-package doom-modeline
    :config
    (doom-modeline-mode 1)
    :custom
    (doom-modeline-hud t)))

(provide 'modeline)
;;; modeline.el ends here
