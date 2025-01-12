;;; init-search.el --- Search                        -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Duan Ning

;; Author: Duan Ning <jousimies@DNs-Air.local>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
;; (rg-enable-default-bindings)
;; https://github.com/dajva/rg.el/issues/142#issuecomment-1452525225
(use-package rg
  :straight t
  :bind ("s-s r" . rg)
  :custom
  (rg-group-result t)
  (rg-show-columns t))

(with-eval-after-load 'rg
  (add-to-list 'rg-finish-functions (lambda (buffer _) (pop-to-buffer buffer)))
  (add-to-list 'display-buffer-alist '("^\\*rg\\*"
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (window-width . 0.5))))

(setopt my/browser-engines
        '((DoubanMovie . "https://search.douban.com/movie/subject_search?search_text=")
          (DoubanBook . "https://search.douban.com/book/subject_search?search_text=")
          (Zhihu . "https://www.zhihu.com/search?type=content&q=")
          (Google . "https://www.google.com/search?q=")
          (Scholar . "https://scholar.google.com/scholar?q=")
          (SemanticScholar . "https://www.semanticscholar.org/search?q=")
          (Github . "https://github.com/search?q=")
          (Youtube . "http://www.youtube.com/results?aq=f&oq=&search_query=")
	  (Bilibili . "https://search.bilibili.com/all?keyword=")
	  (WikiPedia_en . "https://en.wikipedia.org/w/index.php?search=")
	  (Annas-Archvie . "https://annas-archive.org/search?q=")))

(defmacro my/define-search-functions ()
  "Dynamically define search functions for each search engine in `my/browser-engines`."
  `(progn
     ,@(mapcar (lambda (engine)
                 (let* ((engine-name (car engine))
                        (function-name (intern (format "my/search-%s" (downcase (symbol-name engine-name))))))
                   `(defun ,function-name (query)
                      ,(format "Search for QUERY using the %s engine." engine-name)
                      (interactive
                       (list (let ((default-query
                                    (if (and (eq system-type 'darwin)
                                             (featurep 'emt))
                                        (emt-word-at-point-or-forward)
                                      (thing-at-point 'word t))))
                               (if (region-active-p)
                                   (buffer-substring-no-properties (region-beginning) (region-end))
                                 (read-string (format "[%s] Enter search terms (default: %s): " ,(symbol-name engine-name) default-query))))))
                      (let ((search-url (concat ,(cdr engine) (url-encode-url query))))
                        (browse-url search-url))
                      (when (region-active-p)
                        (deactivate-mark)))))
               my/browser-engines)))

(add-hook 'on-first-input-hook (lambda ()
								 (my/define-search-functions)))

(use-package grab-mac-link
  :straight t
  :commands grab-mac-link-dwim grab-mac-link-safari-1)

;;;###autoload
(defun my/link-grab ()
  (interactive)
  (grab-mac-link-dwim 'safari))

;; https://gist.github.com/tkhoa2711/ef99938c8752ca3e52c2
(defun get-ip-address (&optional dev)
	"Get the IP-address for device DEV (default: eth0) of the current machine."
	(let ((dev (if dev dev "en0")))
      (format-network-address (car (network-interface-info dev)) t)))

(use-package simple-httpd
  :straight t
  :bind ("M-g d" . httpd-serve-directory)
  :custom
  (httpd-host (format-network-address
			   (car (network-interface-info "en0"))
			   t)))


(provide 'init-search)
;;; init-search.el ends here
