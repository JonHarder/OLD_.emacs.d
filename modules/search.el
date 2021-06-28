;;; Code:
(require 'thingatpt)
(require 'contrib "~/.emacs.d/contrib")

(defvar search-engines
  '((google . "https://google.com/search?q=")
    (ddg . "https://ddg.gg/?q=")
    (stack-overflow . "https://stackoverflow.com/search?q=")
    (bible-gateway . "https://www.biblegateway.com/passage/?version=ESV&search=")))

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

(defun modules/search--load (config)
  "Load search engine functionality as defined by CONFIG."

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
    (how-do-i "stack-overflow"))

  (defun how-do-i-bible ()
    "Bible Gateway search."
    (interactive)
    (how-do-i "bible-gateway")))

(provide 'search)
;;; search.el ends here
