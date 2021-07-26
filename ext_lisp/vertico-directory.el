;;; vertico-directory --- -*- lexical-binding: t -*-


;;; Commentary:
;;; Taken from https://github.com/minad/vertico/blob/main/extensions/vertico-directory.el
;;; and as such all credit and ownership belongs to minad, the author of vertico
;;; just using what's necessary here until the package maintainer decides what to do
;;; with extensions for vertico https://github.com/minad/vertico/issues/83


;;; Code:
(require 'vertico)

(defun vertico-directory--completing-file-p ()
  "Return non nil when completing file names."
  (eq 'file
      (completion-metadata-get
       (completion-metadata
        (buffer-substring (minibuffer-prompt-end)
                          (max (minibuffer-prompt-end) (point)))
        minibuffer-completion-table
        minibuffer-completion-predicate)
       'category)))

(defun vertico-directory-up ()
  "Delete directory before point."
  (interactive)
  (when (and (eq (char-before) ?/)
             (vertico-directory--completing-file-p))
    (save-excursion
      (goto-char (1- (point)))
      (when (search-backward "/" (minibuffer-prompt-end) t)
        (delete-region (1+ (point)) (point-max))
        t))))
       
(defun vertico-directory-delete-char ()
  "Delete directory or char before point."
  (interactive)
  (unless (vertico-directory-up)
    (call-interactively #'backward-delete-char)))

(defun vertico-directory-delete-word ()
  "Delete directory or word before point."
  (interactive)
  (unless (vertico-directory-up)
    (let ((pt (point)))
      (forward-word -1)
      (delete-region pt (point)))))

(defun vertico-directory-enter()
  "Enter directory or exiit completion with current candidate."
  (interactive)
  (if (and (>= vertico--index 0)
           (string-suffix-p "/" (vertico--candidate))
           (vertico-directory--completing-file-p))
      (vertico-insert)
    (vertico-exit)))

(provide 'vertico-directory)
;;; vertico-directory.el ends here
