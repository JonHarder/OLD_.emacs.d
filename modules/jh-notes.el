;;; package --- Notes -*- lexical-binding: t -*-

;;; Commentary:
;;; Configure note taking system


;;; Code:
(require 'use-package)

(use-package zk
  :custom
  (zk-directory "~/Dropbox/Notes")
  (zk-file-extension "org")
  :config
  (zk-setup-auto-link-buttons)
  (zk-setup-embark)
  (defun zk-org-try-to-follow-link (fn &optional arg)
    (let ((org-link-search-must-match-exact-headline t))
      (condition-case nil
          (apply fn arg)
        (error (zk-follow-link-at-point)))))
  (advice-add 'org-open-at-point :around #'zk-org-try-to-follow-link))

  (provide 'jh-notes)
;;; jh-notes.el ends here.
