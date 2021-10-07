;;; elfeed-config.el --- elfeed configuration
;;; Commentary:
;;; Code:

;; Pull in the elfeed package
(use-package elfeed)

(global-set-key (kbd "C-c e") #'elfeed)

;; List of all feeds that I should fetch and care about
;; The cdr (tail of list) will be symbols attached to anything coming from that
;; particular feed.
(setq elfeed-feeds '(("https://karl.hallsby.com/feed.xml" personal blog)
                     ("https://xkcd.com/atom.xml" comic webcomic)
                     ("https://guix.gnu.org/feeds/blog.atom" guix reproducible)
                     ("https://protesilaos.com/codelog.xml" emacs blog)))

;; Prettify the elfeed buffer, making some things easier to read
(use-package elfeed-goodies)
(require 'elfeed-goodies)
(elfeed-goodies/setup)

(provide 'elfeed-config)
;;; elfeed-config.el ends here
