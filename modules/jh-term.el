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
          (unless (one-window-p)
            (delete-window))
          (kill-buffer buf)
          (message "VTerm closed."))))))

(use-package vterm
  :commands vterm
  :hook (vterm-mode . jh/vterm-init-hook)
  :custom
  ; (vterm-timer-delay 0.01)
  (vterm-timer-delay nil)
  (vterm-shell "/opt/homebrew/bin/fish")
  :config
  (defun jh/vterm ()
    (interactive)
    (vterm)
    (when (equal current-prefix-arg '(4))
        (delete-other-windows)))
  (add-hook 'vterm-mode-hook
            (lambda ()
              (set-process-sentinel (get-buffer-process (buffer-name)) #'vterm--kill-vterm-buffer-and-window)))
  :general
  (:states 'normal
   :prefix "SPC"
   "a v" 'jh/vterm)
  (:keymaps 'vterm-mode-map
   :states '(normal insert)
   "M-v" 'vterm-yank))

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))
(provide 'jh-term)
;;; jh-term.el ends here
