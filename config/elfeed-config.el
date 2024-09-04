;;; elfeed-config.el --- elfeed configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Pull in the elfeed package
(use-package elfeed
  :ensure t
  :bind (("C-c f" . #'elfeed))
  :custom
  ;; List of all feeds that I should fetch and care about
  ;; The cdr (tail of list) will be symbols attached to anything coming from that
  ;; particular feed.
  (elfeed-feeds '(("https://karl.hallsby.com/feed.xml" personal blog)
                  ("https://xkcd.com/atom.xml" comic webcomic)
                  ("https://guix.gnu.org/feeds/blog.atom" guix reproducible)
                  ("https://protesilaos.com/codelog.xml" emacs blog)
                  ("https://ambrevar.xyz/atom.xml" emacs guix reproducible blog)
                  ("https://christine.website/blog.rss" nixos reproducible blog)
                  ("https://ag91.github.io/rss.xml" emacs nyxt blog)
                  ("https://nyxt.atlas.engineer/feed" nyxt blog)
                  ("https://rss.acm.org/technews/TechNews.xml" ACM research news)
                  ("https://wingolog.org/feed/atom" guile compilers blog)
                  ("https://karthinks.com/index.xml" emacs blog)
                  ("https://nickw.io/api/rss.xml" compilers systems blog)
                  ("https://www.cs.cornell.edu/~asampson/blog.xml" systems languages research blog)
                  ("https://spritely.institute/feed.xml" systems guile guix blog)
                  ("https://fasterthanli.me/index.xml" systems blog)
                  ("https://ferd.ca/feed.rss" systems erlang languages blog)
                  ("https://tymoon.eu/api/reader/atom" languages lisp blog)
                  ("https://atillahallsby.com/feed/" blog rhetoric research)))
  ;; NOTE: Make elfeed use Emacs' built-in url-retrieve function rather than cURL.
  ;; This is significantly faster, but only works on Emacsen running on Linux
  ;; and Emacsen that are compiled with GNUTLS support
  ;; (see system-configuration-features).
  (elfeed-use-curl 'nil))

;; Prettify the elfeed buffer, making some things easier to read
(use-package elfeed-goodies
  :ensure t
  :after (elfeed)
  :config
  (elfeed-goodies/setup))

(provide 'elfeed-config)
;;; elfeed-config.el ends here
