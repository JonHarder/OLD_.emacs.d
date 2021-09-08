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
  

(provide 'academic)
;;; academic.el ends here
