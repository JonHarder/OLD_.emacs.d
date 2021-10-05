;;; buffers.el --- configuration for ibuffer mostly -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(use-package ibuffer
  :commands ibuffer
  :general
  (:states 'normal
   :keymaps 'ibuffer-mode-map
   "TAB" 'ibuffer-forward-filter-group
   "RET" 'ibuffer-visit-buffer
   "j" 'ibuffer-forward-line
   "k" 'ibuffer-backward-line
   "d" 'ibuffer-mark-for-delete
   "x" 'ibuffer-do-kill-on-deletion-marks
   "m" 'ibuffer-mark-forward
   "t" 'ibuffer-toggle-marks
   "u" 'ibuffer-unmark-forward
   "s m" 'ibuffer-do-sort-by-major-mode
   "s f" 'ibuffer-do-sort-by-filename/process
   "/ RET" 'ibuffer-filter-by-mode
   "/ SPC" 'ibuffer-filter-chosen-by-completion
   "/ /" 'ibuffer-filter-disable
   "J" 'ibuffer-jump-to-buffer
   "S" 'ibuffer-do-save
   "D" 'ibuffer-do-delete)
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
              (ibuffer-auto-mode 1))))

(provide 'buffers)
;;; buffers.el ends here
