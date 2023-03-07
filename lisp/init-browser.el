;; init-browser.el --- browser *- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; eww
;; Install readability first.
;; npm install -g readability-cli
;; (setq eww-retrieve-command '("readable"))

;; Another choice `websearch'.
;; Search engine
(use-package engine-mode
  :hook (on-first-input . engine-mode)
  :config
  (defengine google "https://google.com/search?q=%s"
             :keybinding "g"
             :docstring "Search Google.")
  (defengine wikipedia "https://en.wikipedia.org/wiki/Special:Search?search=%s"
             :keybinding "w"
             :docstring "Search Wikipedia.")
  (defengine github "https://github.com/search?ref=simplesearch&q=%s"
             :keybinding "h"
             :docstring "Search GitHub.")
  (defengine youtube "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
             :keybinding "y"
             :docstring "Search YouTube.")
  (defengine moviedouban "https://search.douban.com/movie/subject_search?search_text=%s"
             :keybinding "m"
             :docstring "Search Moive DouBan.")
  (defengine bookdouban "https://search.douban.com/book/subject_search?search_text=%s"
             :keybinding "b"
             :docstring "Search Book DouBan.")
  (defengine zhihu "https://www.zhihu.com/search?type=content&q=%s"
             :keybinding "z"
             :docstring "Search Zhihu."))

(my/space-leader-def
  "s" '(:ignore t :wk "Search")
  "sb" '(engine/search-bookdouban :wk "Book")
  "ss" '(engine/search-google :wk "Google")
  "sg" '(engine/search-github :wk "Github")
  "sw" '(engine/search-wikipedia :wk "Wiki")
  "sm" '(engine/search-moviedouban :wk "Movie")
  "sz" '(engine/search-zhihu :wk "Zhihu")
  "sr" '(rg :wk "rg")
  "sl" '(consult-git-grep :wk "git"))

(provide 'init-browser)
;;; init-browser.el ends here.
