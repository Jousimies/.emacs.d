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

(use-package rg
  :bind ("C-c s". rg)
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

(setq my/browser-engines
      '((DoubanMovie . "https://search.douban.com/movie/subject_search?search_text=")
        (DoubanBook . "https://search.douban.com/book/subject_search?search_text=")
        (Zhihu . "https://www.zhihu.com/search?type=content&q=")
        (Google . "https://www.google.com/search?q=")
        (Scholar . "https://scholar.google.com/scholar?hl=zh-CN&as_sdt=0%2C33&q=")
        (Github . "https://github.com/search?q=")
        (Youtube . "http://www.youtube.com/results?aq=f&oq=&search_query=")
		(Bilibili . "https://search.bilibili.com/all?keyword=")))

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
;; (global-set-key (kbd "C-c w") #'my/search)
;; (global-set-key (kbd "s-s") #'my/search)

(use-package grab-mac-link
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
  :bind ("M-g h" . httpd-serve-directory)
  :config
  (setq httpd-host (format-network-address
					(car (network-interface-info "en0"))
					t)))


(provide 'init-search)
;;; init-search.el ends here
