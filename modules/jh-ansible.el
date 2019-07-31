;;; ansible --- Summary


;;; Commentary:

;;; Code:
(defvar ansible/ansible-executible "~/bin/ans")
(defvar ansible/playbooks-directory (getenv "ANSIBLE_PLAYBOOK_DIR"))


(defun ansible/make-var (value)
  "Format VALUE as an ansible extra var."
  (let ((formatted
         (if (string-match-p "=" value)
             value
           (format "%s=yes" value))))
    (format "--extra-vars='%s'" formatted)))


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
    (switch-to-buffer buffer)))


(define-transient-command ansible/devtest-update-transient ()
 "Run a playbook with some extra vars"
 ["Arguments"
  ("-n" "Devtest Var" "devtest_var_number=")
  ("-u" "Ubuntu 18.04?" "ubuntu_1804")]
 ["Actions"
  ("r" "Run playbook" ansible/run-playbook)])


(provide 'jh-ansible)
;;; jh-ansible.el ends here
