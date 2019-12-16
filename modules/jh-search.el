;;; search --- Summary
;; 

;;; Commentary:

;;; Code:


(require 'contrib "~/.emacs.d/contrib")

(defun modules/search--load (config)
  "Load search engine functionality as defined by CONFIG."

  (defvar search-engines
    '((google . "https://google.com/search?q=")
      (ddg . "https://ddg.gg/?q=")
      (stack-overflow . "https://stackoverflow.com/search?q=")))
  
  (defun how-do-i (engine search-term)
    "Use a specified search ENGINE to query your SEARCH-TERM."
    ;; use region if active for search-term
    (interactive
     (list
      (completing-read "Enigne: " (contrib/alist-keys search-engines))
      (read-string "Search: ")))
    (let ((url (cdr (assoc (intern engine) search-engines))))
      (browse-url (concat url search-term))))
  
  (defun how-do-i-google (search-term)
    "Google search for SEARCH-TERM."
    (interactive "sSearch: ")
    (how-do-i "google" search-term))
  
  (defun how-do-i-ddg (search-term)
    "DuckDuckGo search for SEARCH-TERM."
    (interactive "sSearch: ")
    (how-do-i "ddg" search-term))
  
  (defun how-do-i-so (search-term)
    "Stack Overflow search for SEARCH-TERM."
    (interactive "sSearch: ")
    (how-do-i "stack-overflow" search-term)))

(provide 'jh-search)
;;; jh-search.el ends here
