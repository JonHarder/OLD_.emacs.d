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

(defun add-citation-footnote (reference page)
  "Insert org footnote using `consult-bibtex' prompting for REFERENCE and PAGE."
  (interactive (list
                (consult-bibtex--read-entry)
                (read-string "Page: ")) org)
  (save-excursion
    (call-interactively #'org-footnote-action)
    (insert (format " \\cite[%s]{%s}" page reference)))
  (search-forward "]"))

(general-define-key
 :keymaps 'org-mode-map
 :states 'normal
 :prefix ","
 "f" #'add-citation-footnote)

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
  :after consult
  :commands (consult-bibtex)
  :straight '(consult-bibtex :host github
                             :repo "mohkale/consult-bibtex")
  :config
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist '(bibtex-completion . consult-bibtex-embark-map))))

(provide 'academic)
;;; academic.el ends here
