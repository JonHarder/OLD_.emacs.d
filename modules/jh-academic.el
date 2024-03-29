;;; academic --- Configuration for anything related to writing academic papers
;;; Commentary:

;;; Code:

;; (use-package org-contrib
;;   :after org
;;   :config
;;   (require 'ox-bibtex)
;;   :custom
;;   (org-latex-pdf-process '("latexmk -pdf -outdir=%o %f"))
;;   (reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))
;;   (bib-files-directory '("~/Dropbox/bibliography/references.bib"))
;;   (bibtex-dialect 'biblatex))
(require 'use-package)

(use-package org-ref
  :after org
  :custom
  (org-latex-pdf-process '("latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))
  (reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))
  (bib-files-directory '("~/Dropbox/bibliography/references.bib"))
  (bibtex-dialect 'biblatex)
  (org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib"))
  (org-ref-bibliography-notes '("~/Dropbox/bibliography/notes.org"))
  (bibtex-completion-bibliography "~/Dropbox/bibliography/references.bib")
  (bibtex-completion-pdf-open-function
   (lambda (fpath)
    (start-process "open" "*open*" "open" fpath))))

(use-package ebib
  :custom
  (ebib-preload-bib-files '("~/Dropbox/bibliography/references.bib")))
  
(use-package consult-bibtex
  :after (consult general)
  :commands (consult-bibtex add-citation-footnote)
  :straight '(consult-bibtex :host github
                             :repo "mohkale/consult-bibtex")
  :config
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist '(bibtex-completion . consult-bibtex-embark-map)))
  (defun add-citation-footnote (reference page)
    "Insert org footnote using `consult-bibtex' prompting for REFERENCE and PAGE."
    (interactive (list
                  (consult-bibtex--read-entry)
                  (read-string "Page: ")))
    (save-excursion
      (call-interactively #'org-footnote-action)
      (insert (format " \\cite[%s]{%s}" page reference)))
    (search-forward "]"))
  (general-define-key
   :keymaps 'org-mode-map
   :states 'normal
   :prefix ","
   "f" #'add-citation-footnode))

(use-package citar
  :custom
  (citar-bibliography '("~/Dropbox/bibliography/references.bib")))

(use-package citar-org
  :disabled t
  :bind
  (:map org-mode-map
        ("C-c b" . #'org-cite-insert))
  :custom
  (org-cite-global-bibliography '("~/Dropbox/bibliography/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))

(defvar jh/book-notes-dir "~/Dropbox/BookNotes")

(with-eval-after-load 'consult
  (defun jh/book-grep (&optional dir)
    "Grep for a textual match in DIR using `consult-grep'."
    (interactive)
    (let ((dir (if (equal current-prefix-arg nil)
                   jh/book-notes-dir
                 (read-directory-name "In directory: "))))
      (consult-grep dir))))

(provide 'jh-academic)
;;; academic.el ends here
