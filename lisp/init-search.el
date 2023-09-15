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
  ;; :bind ("C-c s g" . rg)
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
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)))

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
               '("Youtube" . [simple-query
                              "www.youtube.com"
                              "http://www.youtube.com/results?aq=f&oq=&search_query=" ""])))

(use-package grab-mac-link
  :commands grab-mac-link-dwim grab-mac-link-safari-1)

;;;###autoload
(defun my/link-grab ()
  (interactive)
  (grab-mac-link-dwim 'safari))
(global-set-key (kbd "C-<f10>") 'my/link-grab)

(use-package simple-httpd
  :bind ("M-g h" . httpd-serve-directory))

(provide 'init-search)
;;; init-search.el ends here.
