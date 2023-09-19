;; init-search.el --- Search. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package isearch
  :defer t
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format nil)
  (setq lazy-count-suffix-format "   (%s/%s)"))

(use-package rg
  :load-path ("packages/rg.el/" "packages/Emacs-wgrep")
  :bind ("C-c s". rg-menu)
  :config
  ;; https://github.com/dajva/rg.el/issues/142#issuecomment-1452525225
  (add-to-list 'rg-finish-functions (lambda (buffer _) (pop-to-buffer buffer)))
  (rg-enable-default-bindings)
  (setq rg-group-result t)
  (setq rg-show-columns t))

(use-package help
  :defer t
  :config
  (setq help-window-select 'always)
  (setq help-window-keep-selected t))

(use-package helpful
  :load-path "packages/helpful/"
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)))

(add-to-list 'load-path "~/.emacs.d/packages/elisp-refs/")
(add-to-list 'load-path "~/.emacs.d/packages/elisp-demos/")
(autoload #'elisp-demos-advice-helpful-update "elisp-demos" nil t)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(use-package webjump
  :bind ("C-c w" . webjump)
  :config
  (add-to-list 'webjump-sites
               '("ZhiHu" . [simple-query
                            "www.zhihu.com"
                            "www.zhihu.com/search?type=content&q=" ""]))
  (add-to-list 'webjump-sites
               '("Github" . [simple-query
                             "github.com"
                             "github.com/search?q=" ""]))
  (add-to-list 'webjump-sites
               '("Douban Books" . [simple-query
                                   "search.douban.com"
                                   "https://search.douban.com/book/subject_search?search_text=" ""]))
  (add-to-list 'webjump-sites
               '("Douban Movies" . [simple-query
                                    "search.douban.com"
                                    "https://search.douban.com/movie/subject_search?search_text=" ""]))
  (add-to-list 'webjump-sites
               '("Scholar" . [simple-query
                              "scholar.google.com"
                              "https://scholar.google.com/scholar?hl=zh-CN&as_sdt=0%2C33&q=" ""]))
  (add-to-list 'webjump-sites
               '("Youtube" . [simple-query
                              "www.youtube.com"
                              "http://www.youtube.com/results?aq=f&oq=&search_query=" ""])))

(use-package grab-mac-link
  :load-path "packages/grab-mac-link.el/"
  :commands grab-mac-link-dwim grab-mac-link-safari-1)

;;;###autoload
(defun my/link-grab ()
  (interactive)
  (grab-mac-link-dwim 'safari))
(global-set-key (kbd "C-<f10>") 'my/link-grab)

(use-package simple-httpd
  :load-path "packages/simple-httpd/"
  :bind ("M-g h" . httpd-serve-directory))

(use-package bard
  :load-path "packages/Bard.el/"
  :bind ("C-c a b" . bard-chat)
  :config
  (setq bard-http-proxy "http://127.0.0.1:8118"))

(provide 'init-search)
;;; init-search.el ends here.
