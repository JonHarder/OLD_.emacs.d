(defun modules/zettelkasten--load (config)
  "Configure zettelkastel-like note taking workflow using CONFIG."

  (defvar zettelkasten/notes-directory "~/zettelkasten/")

  (defun zettelkasten/--get-file-name (name)
    (let* ((name-prefix (format-time-string "%Y%m%d%H%M%S"))
           (safe-name (combine-and-quote-strings (split-string name) "_"))
           (fname (concat name-prefix "_" safe-name ".org")))
      (concat (file-name-as-directory zettelkasten/notes-directory) fname)))

  (defun zettelkasten/--get-name-suffix (file)
    (file-name-sans-extension
     (combine-and-quote-strings (cdr (split-string file "_")) "_")))
    

  (defun zettelkasten/new-note (name)
    (interactive "MName: ")
    (let ((fname (zettelkasten/--get-file-name name)))
      (find-file fname)
      (insert (concat
               "#+TITLE: " (capitalize name) "\n"
               "#+TAGS: \n\n\n"
               "* links\n"
               "  - "))
      (goto-char (point-min))))


  (defun zettelkasten/search-notes ()
    "Search for notes given tags?"
    (interactive))


  (defun zettelkasten/insert-link ()
    "Prompt for another zettelkasten note and insert a link to it in the current buffer."
    (interactive)
    (let* ((files (cddr (directory-files zettelkasten/notes-directory)))
           (files-alist (mapcar (lambda (f) (cons (zettelkasten/--get-name-suffix f) f)) files))
           (file (completing-read "Note: " (mapcar #'car files-alist)))
           (filepath (concat (file-name-as-directory zettelkasten/notes-directory) (alist-get file files-alist))))
      (insert (format "[[%s][%s]]" filepath file)))))
    

(provide 'zettelkasten)
;;; zettelkasten.el ends here
