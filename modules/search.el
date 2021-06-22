(require 'contrib "~/.emacs.d/contrib")

(defun modules/search--load (config)
  "Load search engine functionality as defined by CONFIG."

  (defvar search-engines
    '((google . "https://google.com/search?q=")
      (ddg . "https://ddg.gg/?q=")
      (stack-overflow . "https://stackoverflow.com/search?q=")))
  
  (defun how-do-i (engine)
    "Use a specified search ENGINE to query your SEARCH-TERM."
    ;; use region if active for search-term
    (interactive
     (list
      (completing-read "Enigne: " (contrib/alist-keys search-engines))))
    (let ((search-term (read-string "Search: " (word-at-point)))
          (browse-url-browser-function 'eww)
          (url (cdr (assoc (intern engine) search-engines))))
      (browse-url (concat url search-term))))
  
  (defun how-do-i-google ()
    "Google search."
    (interactive)
    (how-do-i "google"))
  
  (defun how-do-i-ddg ()
    "DuckDuckGo search."
    (interactive)
    (how-do-i "ddg"))
  
  (defun how-do-i-so ()
    "Stack Overflow search."
    (interactive)
    (how-do-i "stack-overflow")))
