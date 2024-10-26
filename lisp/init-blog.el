;;; init-blog.el --- Blog with org publish           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Duan Ning

;; Author: Duan Ning <duan_n@outlook.com>
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

(defvar my/publish-directory "~/Blogs/")

(with-eval-after-load 'ox-publish
  (setq org-publish-project-alist `(("site"
									 :base-directory ,website-directory
									 :base-extension "org"
									 :recursive nil
									 :publishing-directory ,my/publish-directory
									 :publishing-function org-html-publish-to-html)

									("posts"
									 :base-directory ,(expand-file-name "posts" website-directory)
									 :base-extension "org"
									 :publishing-directory ,(expand-file-name "posts" my/publish-directory)
									 :publishing-function org-html-publish-to-html
									 :with-author t
									 :auto-sitemap t
									 :sitemap-filename "index.org"
									 :sitemap-title "posts"
									 :sitemap-sort-files anti-chronologically
									 :sitemap-format-entry taingram--sitemap-dated-entry-format)

									("static"
									 :base-directory ,website-directory
									 :base-extension "css\\|js\\|txt\\|jpg\\|gif\\|png"
									 :recursive t
									 :publishing-directory  ,my/publish-directory
									 :publishing-function org-publish-attachment)

									("personal-website" :components ("site" "posts" "static"))))

  ;; https://git.sr.ht/~taingram/taingram.org/tree/master/item/publish.el
  (defun taingram--sitemap-dated-entry-format (entry style project)
    "Sitemap PROJECT ENTRY STYLE format that includes date."
    (let ((filename (org-publish-find-title entry project)))
      (if (= (length filename) 0)
          (format "*%s*" entry)
        (format "{{{timestamp(%s)}}}   [[file:%s][%s]]"
                (format-time-string "%Y-%m-%d"
                                    (org-publish-find-date entry project))
                entry
                filename))))

  (defun my/ox-publish-move-images (origin publish)
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[file:\\(.*?\\)\\]\\]" nil t)
        (let* ((image-path (match-string 1))
               (picture-name (car (last (split-string image-path "/"))))
               (new-path (concat my/publish-directory "static/" picture-name)))
          (copy-file image-path new-path t)))))

  (defun my/ox-publish-replace-src-path (origin publish)
    "Replace image paths in the HTML file."
    (interactive)
    (message "%s%s" origin publish)
    (with-temp-buffer
      (insert-file-contents publish)
      (goto-char (point-min))
      (while (re-search-forward (concat "file://" (expand-file-name my-galaxy) "/pictures/") nil t)
        (replace-match "../static/"))
      (write-region (point-min) (point-max) publish)))
  (add-hook 'org-publish-after-publishing-hook 'my/ox-publish-move-images)
  (add-hook 'org-publish-after-publishing-hook 'my/ox-publish-replace-src-path))

(with-eval-after-load 'ox-html
  (setq org-export-global-macros
        '(("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@")))
  (setq org-html-preamble t)
  (setq org-html-preamble-format
		'(("en" "<a href=\"/index.html\" class=\"button\">Home</a>
               <a href=\"/posts/index.html\" class=\"button\">Posts</a>
               <a href=\"/about.html\" class=\"button\">About</a>
               <hr>")))

  (setq org-html-postamble t)

  (setq org-html-postamble-format
        '(("en" "<hr><div class=\"info\"> <span class=\"created\">Created with %c on MacOS</span>
 <span class=\"updated\">Updated: %d</span> </div>")))

  (setq org-html-head-include-default-style nil)

  (setq org-html-head
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/style.css\" />
         <script src=\"js/copy.js\"></script> "))

(provide 'init-blog)
;;; init-blog.el ends here
