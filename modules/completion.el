;;; completion --- Summary
;; configure completion engine : Ivy

;;; Commentary:

;;; Code:
(use-package ivy
  :after (counsel general evil)
  :ensure t
  :init
  (setq ivy-use-virtual-buffers 1
        enable-recursive-minibuffers t
        ivy-height 20)
  :config
  (ivy-mode 1))


(use-package ivy-rich
  :after ivy
  :ensure t
  :init
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-path-style 'abbrev
        ivy-format-function #'ivy-format-function-line)
  :config
  (ivy-rich-mode))

(provide 'completion)
;;; completion.el ends here
