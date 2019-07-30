;;; core --- Summary
;; Core configuration setup

;;; Commentary:
;; This sets up the basics of the configuration including use-package,
;; and basic settings (or things that I can't find a better place to put them into)

;;; Code:
(use-package git-gutter
  :straight t
  :config
  (global-git-gutter-mode +1))


(use-package magit
  :straight t)

(provide 'jh-git)
;;; jh-git.el ends here
