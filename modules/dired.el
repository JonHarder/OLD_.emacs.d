(defun modules/dired--load (config)
  "Load configuration for dired, using CONFIG."
  (straight-use-package 'diredfl)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "-lahoG")

  (use-package dired-single
    :config
    (evil-define-key 'normal dired-mode-map
      "h" 'dired-single-up-directory
      "l" 'dired-single-buffer
      (kbd "RET") 'dired-single-buffer))

  (evil-define-key 'normal dired-mode-map
    "s" 'eshell
    "f" 'find-file
    "o" 'other-window
    (kbd "RET") 'dired-find-file-other-window
    "q" (lambda () (interactive) (quit-window t)))

  (defun dired-side ()
    (interactive)
    (let ((dir (if (eq (vc-root-dir) nil)
                   (dired-noselect default-directory)
                 (dired-noselect (vc-root-dir)))))
      (display-buffer-in-side-window
       dir
       `((side . left)
         (slot . 0)
         (window-width . 0.18)
         (window-parameters . ((no-delete-other-windows . t)
                               (mode-line-format . (" " "%b"))))))
      (with-current-buffer dir
        (rename-buffer "*Dired-Side*"))
      (select-window (get-buffer-window "*Dired-Side*") nil)))

  (defun dired-side-toggle ()
    (interactive)
    (let ((buf (get-buffer "*Dired-Side*")))
      (if buf
          (with-current-buffer "*Dired-Side*"
            (kill-buffer-and-window))
        (dired-side))))
  

  (use-package dired-collapse)
  (use-package dired-rainbow)

  (use-package dired-hide-dotfiles
    :hook (dired-mode . dired-hide-dotfiles-mode)
    :config
    (evil-define-key 'normal dired-mode-map
      "H" 'dired-hide-dotfiles-mode))

  (use-package dired-subtree
    :config
    (evil-define-key 'normal dired-mode-map
      "TAB" 'dired-subtree-toggle
      [?\C-\t] 'dired-subtree-cycle)))
