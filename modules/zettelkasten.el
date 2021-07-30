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
  "DONE"
  (interactive "MName: ")
  (let ((fname (zettelkasten/--get-file-name name)))
    (find-file fname)
    (insert (concat
             "#+TITLE: " (capitalize name) "\n"
             "#+TAGS: \n\n\n"
             "* links\n"
             "  - "))
    (goto-char (point-min))))


(defun zettelkasten/search-notes (search)
  "TODO: needs tag based search. Search for notes given tags?"
  (interactive "sSearch: ")
  (let* ((with-pipes (combine-and-quote-strings (split-string search " ") "|"))
         (search-term (format "(%s)" with-pipes)))
    (rg (format "#\+TAGS: .*%s.*" search-term) "*.org" zettelkasten/notes-directory)))


(defun zettelkasten/insert-back-links (links)
  "TODO: for each link insert into file"
  (dolist (link links)
    (message "%s" link)))
    
(defun zettelkasten/current-tags ()
  "TODO: actually find these from the file."
  (interactive)
  '("work" "tech-debt"))

(defun zettelkasten/update-related-notes ()
  "TODO: Find other notes tagged with the current notes tags.

Should be called upon tag addition."
  (interactive)
  (let* ((current-tags (zettelkasten/current-tags))
         ;;; TODO create this function
         (related (find-the-notes-with-all-tags current-tags)))
    (zettelkasten/insert-back-links related)))

(defun zettelkasten/insert-tag (tag)
  "Add the given TAG to the note file.")


(defun zettelkasten/add-tag (tag)
  "DONE"
  (interactive "sTag: ")
  (zettelkasten/insert-tag tag)
  (zettelkasten/update-related-notes))

(defun zettelkasten/insert-link ()
  "Prompt for another zettelkasten note and insert a link to it in the current buffer."
  (interactive)
  (let* ((files (cddr (directory-files zettelkasten/notes-directory)))
         (files-alist (mapcar (lambda (f) (cons (zettelkasten/--get-name-suffix f) f)) files))
         (file (completing-read "Note: " (mapcar #'car files-alist)))
         (filepath (concat (file-name-as-directory zettelkasten/notes-directory) (alist-get file files-alist))))
    (insert (format "[[%s][%s]]" filepath file))))
    

(provide 'zettelkasten)
;;; zettelkasten.el ends here
