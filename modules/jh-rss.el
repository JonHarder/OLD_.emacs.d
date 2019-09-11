;;; rss --- Summary


;;; Commentary:

;;; Code:
(defun modules/rss--load (config)
  "Configure any rss settings using CONFIG."
  (use-package elfeed
    :config
    (setq-default
     elfeed-feeds '("https://martinfowler.com/feed.atom"))))


(provide 'jh-rss)
;;; jh-rss.el ends here
