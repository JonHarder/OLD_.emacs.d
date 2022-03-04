;;; modeline --- Summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package doom-modeline
  :hook (emacs-startup . doom-modeline-mode)
  :custom
  (doom-modeline-hud t)
  (doom-modeline-buffer-file-name 'relative-from-project)
  (doom-modeline-enable-word-count t)
  (display-time-24hr-format nil))

(display-time-mode t)
(display-battery-mode t)

(provide 'jh-modeline)
;;; jh-modeline.el ends here
