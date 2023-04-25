;; init-Blog.el --- Blog with hugo. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package ox-html
  :after ox
  :config
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

(use-package ox-publish
  :after ox
  :config
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

  (defvar website-directory (expand-file-name "blogs_source" my-galaxy)
    "The source folder of my blog.")

  (defvar my/publish-directory "~/Blogs/")

  (setq org-publish-project-alist
        `(("site"
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

          ("personal-website" :components ("site" "posts" "static")))))

(provide 'init-blog)
;;; init-blog.el ends here.
