;;; rss --- Configuration for aggregating posts from rss links

;;; Commentary:


;;; Code:
(require 'use-package)

(defun modules/rss--load (config)
  "Configure any rss settings using CONFIG."
  (use-package elfeed
    :defer 3
    :config
    (setq-default
     elfeed-feeds '("https://martinfowler.com/feed.atom"
                    "https://emacsredux.com/atom.xml"
                    "https://protesilaos.com/codelog.xml"
                    "https://desiringgod.org/blog.rss"))

    (defun jh/elfeed-load-and-update ()
      "Brings up elfeed UI and then grabs latest posts."
      (interactive)
      (elfeed)
      (elfeed-update))))

(provide 'rss)
;;; rss.el ends here
