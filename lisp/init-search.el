;; init-search.el --- Search. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package isearch
  :bind (:map isearch-mode-map
              ("\C-h" . isearch-delete-char))
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format nil)
  (setq lazy-count-suffix-format "   (%s/%s)"))

(use-package rg
  :load-path ("packages/rg.el/" "packages/Emacs-wgrep")
  :bind ("C-c s". rg-menu)
  :config
  (add-to-list 'display-buffer-alist '("^\\*rg\\*"
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (window-width . 0.5)))
  ;; https://github.com/dajva/rg.el/issues/142#issuecomment-1452525225
  (add-to-list 'rg-finish-functions (lambda (buffer _) (pop-to-buffer buffer)))
  (rg-enable-default-bindings)
  (setq rg-group-result t)
  (setq rg-show-columns t))

(use-package help
  :config
  (setq help-window-select 'other)
  (setq help-window-keep-selected t))

(use-package helpful
  :load-path "packages/helpful/" "packages/elisp-refs/"
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key))
  :config
  (add-to-list 'display-buffer-alist '("\\*helpful"
                                         (display-buffer-in-side-window)
                                         (side . right)
                                         (window-width . 0.5)
                                         (window-parameters
                                          (mode-line-format . none)))))

(add-to-list 'load-path "~/.emacs.d/packages/elisp-refs/")
(add-to-list 'load-path "~/.emacs.d/packages/elisp-demos/")
(autoload #'elisp-demos-advice-helpful-update "elisp-demos" nil t)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(setq my/browser-engines
      '((DoubanMovie . "https://search.douban.com/movie/subject_search?search_text=")
        (DoubanBook . "https://search.douban.com/book/subject_search?search_text=")
        (Zhihu . "https://www.zhihu.com/search?type=content&q=")
        (Google . "https://www.google.com/search?q=")
        (Scholar . "https://scholar.google.com/scholar?hl=zh-CN&as_sdt=0%2C33&q=")
        (Github . "github.com/search?q=")
        (Youtube . "http://www.youtube.com/results?aq=f&oq=&search_query=")))

;;;###autoload
(defun my/search ()
  "Search using the specified engine for the text in the currently selected region or user input."
  (interactive)
  (let* ((selected-engine (completing-read "Choose a search engine: " (mapcar 'car my/browser-engines) nil t))
         (selected-url (cdr (assoc (intern selected-engine) my/browser-engines))))
    (let* ((region (if (region-active-p)
                       (buffer-substring-no-properties (region-beginning) (region-end))
                     (read-string "Enter search terms: ")))
           (encoded-region (url-encode-url region))
           (search-url (concat selected-url encoded-region)))
      (browse-url search-url)))
  (when (region-active-p)
    (deactivate-mark)))
(global-set-key (kbd "C-c w") #'my/search)

(use-package grab-mac-link
  :load-path "packages/grab-mac-link.el/"
  :commands grab-mac-link-dwim grab-mac-link-safari-1)

;;;###autoload
(defun my/link-grab ()
  (interactive)
  (grab-mac-link-dwim 'safari))
(global-set-key (kbd "s-l g") 'my/link-grab)

(use-package simple-httpd
  :load-path "packages/emacs-web-server"
  :bind ("M-g h" . httpd-serve-directory))

(use-package bard
  :load-path "packages/Bard.el/"
  :bind ("C-c a b" . bard-chat)
  :config
  (setq bard-http-proxy "http://127.0.0.1:8118"))

(provide 'init-search)
;;; init-search.el ends here.
