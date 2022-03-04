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
                  "https://desiringgod.org/blog.rss"))
  :general
  (:keymaps 'elfeed-search-mode-map
   :states 'normal
   "RET" 'elfeed-search-show-entry
   "q" 'elfeed-search-quit-window)
  (:keymaps 'elfeed-show-mode-map
   :states 'normal
   "q" 'elfeed-kill-buffer))

(provide 'jh-rss)
;;; jh-rss.el ends here
