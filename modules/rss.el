;;; rss --- Configuration for aggregating posts from rss links

;;; Commentary:


;;; Code:
(require 'use-package)

(use-package elfeed
  :commands elfeed
  :config
  (setq-default
   elfeed-feeds '("https://martinfowler.com/feed.atom"
                  "https://emacsredux.com/atom.xml"
                  "https://protesilaos.com/codelog.xml"
                  "https://desiringgod.org/blog.rss")))

(provide 'rss)
;;; rss.el ends here
