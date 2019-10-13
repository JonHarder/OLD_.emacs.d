;;; ansible --- Summary


;;; Commentary:

;;; Code:
(defvar ansible/ansible-executible "~/bin/ans")
(defvar ansible/playbooks-directory (getenv "ANSIBLE_PLAYBOOK_DIR"))
(defvar ansible/playbook-process nil)


(defun ansible/make-var (value)
  "Format VALUE as an ansible extra var."
  (let ((formatted
         (if (string-match-p "=" value)
             value
           (format "%s=yes" value))))
    (format "--extra-vars='%s'" formatted)))


(defun ansible/continue-playbook ()
  "Continue the running playbook."
  (interactive)
  (if (not (null ansible/playbook-process))
      (progn
        (interrupt-process ansible/playbook-process)
        (process-send-string ansible/playbook-process "c"))
    (message "There is no currently running playbook")))


(defun ansible/abort-playbook ()
  "Abort the running playbook."
  (interactive)
  (if (not (null ansible/playbook-process))
      (progn
        (interrupt-process ansible/playbook-process)
        (process-send-string ansible/playbook-process "a"))
    (message "There is no currently running playbook")))


(defun ansible/run-playbook (&rest args)
  "Run an ansible playbook with ARGS."
  (interactive
   (list (transient-args 'ansible/devtest-update-transient)))
  (let* ((default-directory ansible/playbooks-directory)
         (playbook-files (jh/expand-directory ansible/playbooks-directory))
         (playbook (completing-read "playbook: " playbook-files))
         (process "*ansible-playbook*")
         (buffer "*ansible*")
         (extra-vars (mapcar #'ansible/make-var args)))
    (start-file-process process buffer ansible/ansible-executible (cons playbook args))
    (setq ansible/playbook-process process)
    (switch-to-buffer buffer)))


(define-transient-command ansible/interactive-playbook-transient ()
  "Interact with a running playbook"
  ["Actions"
   ("c" "Continue running playbook" ansible/continue-playbook)
   ("a" "Abort running playbook" ansible/abort-playbook)])


(define-transient-command ansible/devtest-update-transient ()
 "Run a playbook with some extra vars"
 ["Arguments"
  ("-n" "Devtest Var" "devtest_var_number=")
  ("-u" "Ubuntu 18.04?" "ubuntu_1804")]
 ["Actions"
  ("r" "Run playbook" ansible/run-playbook)])


(defun modules/ansible--load (config)
  "Load some general ansible packages and ignore CONFIG."
  (use-package jinja2-mode
    :mode "\\.j2\\'")
  (straight-use-package 'ansible)
  (use-package ansible-doc
    :hook (yaml-mode . ansible-doc-mode))
  (add-to-list 'auto-mode-alist '("hosts" . conf-mode)))


(provide 'jh-ansible)
;;; jh-ansible.el ends here
