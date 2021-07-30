;;; term --- Terminal configuration

;;; Commentary:


;;; Code:
(require 'evil)

(use-package fish-mode
  :ensure t
  :mode "\\.fish\\'")

(defun jh/vterm-init-hook ()
  (setq-local evil-insert-state-cursor 'box)
  (evil-insert-state))

(defun vterm--kill-vterm-buffer-and-window (process event)
  "Kill buffer and window on vterm process termination."
  (when (not (process-live-p process))
    (let ((buf (process-buffer process)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (kill-buffer)
          (message "VTerm closed."))))))

(use-package vterm
  :ensure t
  :commands vterm
  :hook (vterm-mode . jh/vterm-init-hook)
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (set-process-sentinel (get-buffer-process (buffer-name)) #'vterm--kill-vterm-buffer-and-window)))
  :general
  (:keymaps 'vterm-mode-map
   :states '(normal insert)
   "M-v" 'vterm-yank))

(use-package multi-vterm
  :ensure t
  :commands multi-vterm
  :general
  (:states 'normal
   :prefix "SPC"
   "a v" '(multi-vterm :wk "VTerm")))

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))
(provide 'term)
;;; term.el ends here
