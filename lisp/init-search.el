;; init-search.el --- Search. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package rg
  :commands rg
  :config
  (rg-enable-default-bindings)
  (setq rg-group-result t)
  (setq rg-show-columns t))

(use-package help
  :config
  (setq help-window-select 'always)
  (setq help-window-keep-selected t))

(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)))

(autoload #'elisp-demos-advice-helpful-update "elisp-demos" nil t)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(use-package devdocs-browser
  :commands devdocs-browser-install-doc devdocs-browser-open devdocs-browser-open-in
  :config
  (setq devdocs-browser-cache-directory (expand-file-name "cache/devdocs-browser/" user-emacs-directory)))

;; eww
;; Install readability first.
;; npm install -g readability-cli
;; (setq eww-retrieve-command '("readable"))

(use-package engine-mode
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
             :docstring "Search Zhihu.")
  (engine-mode))

(use-package grab-mac-link
  :commands grab-mac-link-dwim grab-mac-link-safari-1
  :bind ("<f4>" . my/link-grab)
  :preface
  (defun my/link-grab ()
    (interactive)
    (grab-mac-link-dwim 'safari)))

;;;###autoload
(defun my/org-insert-web-page-archive ()
  "Insert a file about web page archived locally into an Org file as reference."
  (interactive)
  (let* ((url (car (grab-mac-link-safari-1)))
         (title (cadr (grab-mac-link-safari-1)))
         (ID (format-time-string "%Y%m%dT%H%M%S"))
         (new-title (concat ID "--" title))
         (file-path (concat my/web_archive title ".html"))
         (file-new-path (concat my/web_archive new-title ".html")))
    (save-excursion
      (goto-char (point-max))
      (if (file-exists-p file-path)
          (progn
            (rename-file file-path file-new-path)
            (insert "* ")
            (org-insert-link nil file-new-path title)
            (org-set-property "URL" url)
            (org-set-tags "Reference")
            (my/auto-change-file-paths))
        (message "Please save web page first.")))))

(use-package simple-httpd
  :commands httpd-serve-directory)

(provide 'init-search)
;;; init-search.el ends here.
