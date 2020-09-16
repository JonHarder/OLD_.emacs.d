(defun modules/zettelkasten--load (config)
  "Configure zettelkastel-like note taking workflow using CONFIG."

  (defvar zettelkasten/notes-directory "~/zettelkasten/")

  (defun zettelkasten/--get-file-name (name)
    (let* ((name-prefix (format-time-string "%Y%m%d%H%M%S"))
           (safe-name (shell-quote-argument name))
           (fname (concat name-prefix "_" safe-name ".org")))
      (concat (file-name-as-directory zettelkasten/notes-directory fname))))
    

  (defun zettelkasten/new-note (name)
    (interactive "MName: ")
    (let ((fname (zettelkasten/--get-file-name name)))
      (find-file fname)
      (insert (concat
               "#+TITLE: " name "\n"
               "#+TAGS: " "\n" "\n" "\n"
               "* links" "\n"
               "  - "))
      (beginning-of-buffer)))

  (defun zettelkasten/search-note (tag-name)
    "TODO: find a way to search the zk dir for any note with the given TAG-NAME."
    (interactive "MTag: ")
    (let ((default-directory zettelkasten/notes-directory))
      (rg))))
    

(provide 'zettelkasten)
;;; zettelkasten.el ends here
