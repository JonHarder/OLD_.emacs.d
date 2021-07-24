(require 'transient)
(require 'lsp)

;;; TODO: write function to recognize if modules in file aren't downloaded

(defun jh/terraform-run-async-command (action &rest args)
  "Run the terraform ACTION with optional ARGS asynchronously."
  (let ((command (string-join (cons action (transient-get-value)) " ")))
    (async-shell-command (format "terraform %s" command))))

(defun jh/terraform-apply ()
  "Apply terraform configuration."
  (interactive)
  (jh/terraform-run-async-command "apply"))

(defun jh/terraform-plan ()
  "Plan a terraform configuration."
  (interactive)
  (jh/terraform-run-async-command "plan"))

(defun jh/terraform-get ()
  "Get any non-retrieved modules for the current terraform configuration."
  (interactive)
  (jh/terraform-run-async-command "get"))

(defun jh/terraform-init ()
  "Initialize terraform configuration."
  (interactive)
  (jh/terraform-run-async-command "init"))

(transient-define-prefix jh/terraform-transient-get ()
  ["Arguments"
   ("-u" "Update modules" "-update")]
  ["Actions"
   ("g" "Get" jh/terraform-get)])

(transient-define-prefix jh/terraform-transient-apply()
  ["Arguments"
   ("-a" "Auto Approve" "-auto-approve")
   ("-b" "Backup Path" "-backup=" read-file-name)]
  ["Actions"
   ("a" "Apply" jh/terraform-apply)])

;; (define-transient-command jh/terraform-transient-command ())
(transient-define-prefix jh/terraform-transient-command ()
  "Run a terrafrom command."
  ["Get"
   ("g" "Get" jh/terraform-transient-get)]
  ["Run"
   ("a" "Apply" jh/terraform-transient-apply)
   ("p" "Plan" jh/terraform-plan)
   ("i" "Init" jh/terraform-init)])


(defun modules/terraform--load (config)
  "Install terraform mode and ignore CONFIG."
  (use-package terraform-mode
    :ensure t
    :mode "\\.tf\\'"
    :config
    (use-package terraform-doc
      :ensure t)
    ;; (use-package company-terraform
    ;;   :ensure t)

    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection '("/usr/local/bin/terraform-ls" "serve"))
                      :major-modes '(terraform-mode)
                      :server-id 'terraform-ls))
    (add-hook 'terraform-mode-hook #'lsp)
    (add-hook 'terraform-mode-hook (lambda () (corfu-mode -1)))

    (defun jh/terraform-mode-hook ()
      (company-terraform-init)
      (terraform-format-on-save-mode t))

    (add-hook 'terraform-mode-hook #'jh/terraform-mode-hook)))
