;;; term --- Terminal configuration

;;; Commentary:


;;; Code:
(require 'evil)

(use-package fish-mode
  :mode "\\.fish\\'")

(defun jh/vterm-init-hook ()
  "Personal hook when starting a `vterm' process."
  (setq-local evil-insert-state-cursor 'box)
  (evil-insert-state))

(defun vterm--kill-vterm-buffer-and-window (process event)
  "Kill buffer and window on vterm PROCESS termination, ignore EVENT."
  (when (not (process-live-p process))
    (let ((buf (process-buffer process)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (delete-window)
          (kill-buffer buf)
          (message "VTerm closed."))))))

(use-package vterm
  :commands vterm
  :hook (vterm-mode . jh/vterm-init-hook)
  :custom
  (vterm-timer-delay 0.01)
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (set-process-sentinel (get-buffer-process (buffer-name)) #'vterm--kill-vterm-buffer-and-window)))
  :general
  (:states 'normal
   :prefix "SPC"
   "a v" 'vterm)
  (:keymaps 'vterm-mode-map
   :states '(normal insert)
   "M-v" 'vterm-yank))

(use-package multi-vterm
  :disabled t
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
