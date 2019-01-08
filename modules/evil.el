;;; evil --- Summary
;; My configuration for setting up the vim emulation layer within Emacs.

;;; Commentary:
;; For the most part, this just gets evil, sets some settings, and sets up magit to
;; make use of vim bindings

;;; Code:
;; Vim emulation
(use-package evil
  :ensure t
  :init
  (setq evil-search-module 'evil-search
	evil-ex-complete-emacs-commands nil
	evil-vsplit-window-right t
	evil-split-window-below t
	evil-shift-round nil
	evil-want-C-u-scroll t)
  :config
  (evil-mode))

(use-package evil-magit
  :ensure t)


(provide 'evil)
;;; evil.el ends here
