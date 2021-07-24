;;; buffers.el --- configuration for ibuffer mostly -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:
(defun modules/buffers--load (config)
  "Load configuration for general buffer related settings, using CONFIG."
  (use-package ibuffer
    :ensure t
    :commands ibuffer
    :custom
    (ibuffer-expert t)
    (ibuffer-show-empty-filter-groups nil)
    (ibuffer-saved-filter-groups
       '(("home"
          ("emacs-config" (or (filename . ".emacs.d")
                              (filename . "emacs-config")))
          ("Php" (mode . php-mode))
          ("Term" (mode . vterm-mode))
          ("Org" (or (mode . org-mode)
                     (filename . "OrgMode")))
          ("Magit" (name . "magit"))
          ("Help" (or (name . "\*Help\*")
                      (name . "\*Apropos\*")
                      (name . "\*info\*"))))))
    :config
    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "home")
                (ibuffer-auto-mode 1)))))

(provide 'buffers)
;;; buffers.el ends here
