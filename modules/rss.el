(defun modules/rss--load (config)
  "Configure any rss settings using CONFIG."
  (use-package elfeed
    :defer 3
    :config
    (setq-default
     elfeed-feeds '("https://martinfowler.com/feed.atom"
                    "https://devops.com/feed"
                    "http://reddit.com/r/emacs/.rss"))))
