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

  (use-package multi-vterm)


  (defadvice term-handle-exit
      (after term-kill-buffer-on-exit activate)
    (kill-buffer)))
