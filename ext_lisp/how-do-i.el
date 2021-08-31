;;; how-do-i --- Custom searching framework -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'thingatpt)
(require 'contrib "~/.emacs.d/contrib")

(defvar search-engines
  '(("google" . "https://google.com/search?q=")
    ("ddg" . "https://duckduckgo.com/lite?q=")
    ("stack-overflow" . "https://stackoverflow.com/search?q=")
    ("bible-gateway" . "https://www.biblegateway.com/passage/?version=ESV&search=")))

(defun how-do-i (engine &optional prompt)
  "Use a specified search ENGINE to query, optionally using PROMPT."
  ;; use region if active for search-term
  (interactive
   (list
    (completing-read "Enigne: " (contrib/alist-keys search-engines))))
  (let ((search-term (read-string (or prompt "Search: ") (word-at-point)))
        ;; (browse-url-browser-function 'eww)
        (url (cdr (assoc engine search-engines))))
    (browse-url (concat url search-term))))

;; (defmacro def-engine (name search-template)
;;   "Make a new engine using NAME, searching SEARCH-TEMPLATE."
;;   `(let ((engine (cons ,name ,search-template))
;;          (fname (intern (concat "how-do-i-" ,name))))
;;      (add-to-list 'search-engines engine)
;;      (defun ,fname ()
;;        (concat ,name " Search")
;;        (interactive)
;;        (how-do-i ,name))))

;; (def-engine "dictionary" "https://dictionary.com/browse/")

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
  (how-do-i "bible-gateway" "Bible Reference: "))

(provide 'how-do-i)
;;; search.el ends here
