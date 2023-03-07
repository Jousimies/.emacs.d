;; init-blog.el --- blog *- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package ox-hugo
  :defer 3
  :after ox)

(use-package ox-html
  :after ox
  :config
  (setq org-html-preamble t)
  (setq org-html-preamble-format
        '(("en" "<a href=\"/index.html\" class=\"button\">Home</a>
               <a href=\"/notes/index.html\" class=\"button\">Notes</a>
               <a href=\"/engineering/index.html\" class=\"button\">Engineering</a>
               <a href=\"/movies/index.html\" class=\"button\">Movies</a>
               <a href=\"/books/index.html\" class=\"button\">Books</a>
               <a href=\"/about.html\" class=\"button\">About</a>
               <hr>")))

  (setq org-html-postamble t)

  (setq org-html-postamble-format
        '(("en" "<hr><div class=\"generated\">Created with %c on MacOS</div>")))

  (setq org-html-head-include-default-style nil)

  (setq org-html-head
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"../css/style.css\" />"))

(use-package ox-publish
  :after ox
  :config
  (defvar my/publish-directory "~/shuyi.github.io")

  (setq org-publish-project-alist
        `(("site"
           :base-directory ,website-directory
           :base-extension "org"
           :recursive nil
           :publishing-directory ,my/publish-directory
           :publishing-function org-html-publish-to-html)

          ("notes"
           :base-directory ,(expand-file-name "notes" website-directory)
           :base-extension "org"
           :publishing-directory ,(expand-file-name "notes" my/publish-directory)
           :publishing-function org-html-publish-to-html
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "Notes"
           :sitemap-sort-files anti-chronologically)
          ("books"
           :base-directory ,(expand-file-name "books" website-directory)
           :base-extension "org"
           :publishing-directory ,(expand-file-name "books" my/publish-directory)
           :publishing-function org-html-publish-to-html
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "Books"
           :sitemap-sort-files anti-chronologically)
          ("movies"
           :base-directory ,(expand-file-name "movies" website-directory)
           :base-extension "org"
           :publishing-directory ,(expand-file-name "movies" my/publish-directory)
           :publishing-function org-html-publish-to-html
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "Movies"
           :sitemap-sort-files anti-chronologically)
          ("engineering"
           :base-directory ,(expand-file-name "engineering" website-directory)
           :base-extension "org"
           :publishing-directory ,(expand-file-name "engineering" my/publish-directory)
           :publishing-function org-html-publish-to-html
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "Engineering"
           :sitemap-sort-files anti-chronologically)
          ("static"
           :base-directory ,website-directory
           :base-extension "css\\|txt\\|jpg\\|gif\\|png"
           :recursive t
           :publishing-directory  ,my/publish-directory
           :publishing-function org-publish-attachment)

          ("personal-website" :components ("site" "notes" "books"
                                           "movies" "engineering" "static")))))

(provide 'init-blog)
;;; init-blog.el ends here.
