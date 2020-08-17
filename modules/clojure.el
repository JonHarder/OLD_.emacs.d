(defun modules/clojure--load (config)
  (use-package cider
    :straight t
    :init
    (setq cider-repl-display-help-banner nil)
    :config
    (define-clojure-indent
      (defroutes 'defun)
      (GET 2)
      (POST 2)
      (PUT 2)
      (DELETE 2)
      (HEAD 2)
      (ANY 2)
      (OPTIONS 2)
      (PATCH 2)
      (rfn 2)
      (let-routes 1)
      (context 2))))
