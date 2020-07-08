;;; core --- Summary
;; Configure external terminal emulators to play nice with emacs.

;;; Commentary:
;;; As great as eshell is, there comes a time in every emacser life
;;; where they find themselves reaching for an external shell.
;;; This module aims to make the transition as painless as possible.

;;; Code:
(defun modules/term--load (config)
  "Load term stuff using CONFIG."
  (straight-use-package 'fish-mode)

  (use-package vterm
    :defer 1
    :config
    (defun vterm--kill-vterm-buffer-and-window (process event)
      "Kill buffer and window on vterm process termination."
      (when (not (process-live-p process))
        (let ((buf (process-buffer process)))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (kill-buffer)
              (message "VTerm closed."))))))
    (add-hook 'vterm-mode-hook
              (lambda ()
                (set-process-sentinel (get-buffer-process (buffer-name)) #'vterm--kill-vterm-buffer-and-window))))



  (defadvice term-handle-exit
      (after term-kill-buffer-on-exit activate)
    (kill-buffer)))


(provide 'jh-term)
;;; jh-term.el ends here
