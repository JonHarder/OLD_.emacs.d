(defun modules/personal--load (config)
  "Personal configuration using CONFIG."
  (defun personal-todo ()
    (interactive)
    (save-excursion
      (find-file "~/Org/personal.org")
      (org-agenda-set-restriction-lock 'file)
      (org-agenda-list))))
