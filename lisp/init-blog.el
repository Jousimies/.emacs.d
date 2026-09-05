;; -*- lexical-binding: t; -*-

(require 'subr-x)

(defvar my/publish-directory "~/Repositories/blog-source/")

(defcustom my/blog-source-directory
  (expand-file-name "~/Repositories/blog-source/")
  "Local blog source repository used by GitHub Actions."
  :type 'directory)

(defcustom my/blog-post-author "Jousimies"
  "Default author inserted into new blog posts."
  :type 'string)

(defun my/blog--posts-directory ()
  "Return the blog posts source directory."
  (expand-file-name "org/posts/" my/blog-source-directory))

(defun my/blog--safe-file-name (title)
  "Return a safe Org file name for blog TITLE."
  (concat (string-trim
           (replace-regexp-in-string "[/:*?\"<>|]+" "-" title))
          ".org"))

;;;###autoload
(defun my/blog-new-post (title &optional tags)
  "Create a new blog post in `my/blog-source-directory'.
Optional TAGS is a comma/space separated tag string."
  (interactive
   (list (read-string "Blog title: ")
         (read-string "Tags, optional: ")))
  (when (string-empty-p (string-trim title))
    (user-error "Title cannot be empty"))
  (let* ((posts-dir (my/blog--posts-directory))
         (file (expand-file-name (my/blog--safe-file-name title) posts-dir))
         (date (format-time-string "[%Y-%m-%d %a %H:%M]"))
         (tag-list (seq-filter
                    (lambda (s) (not (string-empty-p s)))
                    (split-string (or tags "") "[,，[:space:]]+")))
         (filetags (when tag-list
                     (concat "#+FILETAGS: :"
                             (mapconcat #'identity tag-list ":")
                             ":\n"))))
    (make-directory posts-dir t)
    (when (file-exists-p file)
      (user-error "Post already exists: %s" file))
    (find-file file)
    (insert (format "#+TITLE: %s\n#+DATE: %s\n#+AUTHOR: %s\n#+OPTIONS: toc:nil\n%s\n"
                    title date my/blog-post-author (or filetags "")))
    (save-buffer)
    (message "Created blog post: %s" file)))

;;;###autoload
(defun my/blog-open-source ()
  "Open blog source repository in Dired."
  (interactive)
  (dired my/blog-source-directory))

;;;###autoload
(defun my/blog-open-posts ()
  "Open blog posts directory in Dired."
  (interactive)
  (dired (my/blog--posts-directory)))

(define-derived-mode my/blog-sync-mode special-mode "Blog-Sync"
  "Major mode for blog sync output buffers.")

;;;###autoload
(defun my/blog-sync ()
  "Auto commit blog source changes and push, triggering GitHub Actions deploy."
  (interactive)
  (let* ((default-directory my/blog-source-directory)
         (message (format "Update blog: %s" (format-time-string "%Y-%m-%d %H:%M")))
         (command
          (format "git add -A && if ! git diff --cached --quiet; then git commit -m %s; fi && git push"
                  (shell-quote-argument message))))
    (let ((buffer (get-buffer-create "*Blog Sync*"))
          (process-connection-type nil))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer))
        (my/blog-sync-mode))
      (pop-to-buffer buffer)
      (start-process-shell-command "blog-sync" buffer command))))

;; (with-eval-after-load 'ox-publish
;;   (setq org-publish-timestamp-directory (expand-file-name "org-timestamps/" cache-directory))
;;   (setq org-publish-project-alist `(("site"
;; 				     :base-directory ,website-directory
;; 				     :base-extension "org"
;; 				     :recursive nil
;; 				     :publishing-directory ,my/publish-directory
;; 				     :publishing-function org-html-publish-to-html)

;; 				    ("posts"
;; 				     :base-directory ,(expand-file-name "posts" website-directory)
;; 				     :base-extension "org"
;; 				     :publishing-directory ,(expand-file-name "posts" my/publish-directory)
;; 				     :publishing-function org-html-publish-to-html
;; 				     :with-author t
;; 				     :auto-sitemap t
;; 				     :sitemap-filename "index.org"
;; 				     :sitemap-title "posts"
;; 				     :sitemap-sort-files anti-chronologically
;; 				     :sitemap-format-entry taingram--sitemap-dated-entry-format)

;; 				    ("static"
;; 				     :base-directory ,website-directory
;; 				     :base-extension "css\\|js\\|txt\\|jpg\\|gif\\|png"
;; 				     :recursive t
;; 				     :publishing-directory  ,my/publish-directory
;; 				     :publishing-function org-publish-attachment)

;; 				    ("personal-website" :components ("site" "posts" "static"))))

;;   ;; https://git.sr.ht/~taingram/taingram.org/tree/master/item/publish.el
;;   (defun taingram--sitemap-dated-entry-format (entry style project)
;;     "Sitemap PROJECT ENTRY STYLE format that includes date."
;;     (let ((filename (org-publish-find-title entry project)))
;;       (if (= (length filename) 0)
;;           (format "*%s*" entry)
;;         (format "{{{timestamp(%s)}}}   [[file:%s][%s]]"
;;                 (format-time-string "%Y-%m-%d"
;;                                     (org-publish-find-date entry project))
;;                 entry
;;                 filename))))

;;   (defun my/ox-publish-move-images (origin publish)
;;     (interactive)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (re-search-forward "\\[\\[file:\\(.*?\\)\\]\\]" nil t)
;;         (let* ((image-path (match-string 1))
;;                (picture-name (car (last (split-string image-path "/"))))
;;                (new-path (concat my/publish-directory "static/" picture-name)))
;;           (copy-file image-path new-path t)))))

;;   (defun my/ox-publish-replace-src-path (origin publish)
;;     "Replace image paths in the HTML file."
;;     (interactive)
;;     (message "%s%s" origin publish)
;;     (with-temp-buffer
;;       (insert-file-contents publish)
;;       (goto-char (point-min))
;;       (while (re-search-forward (concat "file://" (expand-file-name my-galaxy) "/pictures/") nil t)
;;         (replace-match "../static/"))
;;       (write-region (point-min) (point-max) publish)))
;;   (add-hook 'org-publish-after-publishing-hook 'my/ox-publish-move-images)
;;   (add-hook 'org-publish-after-publishing-hook 'my/ox-publish-replace-src-path))

;; (with-eval-after-load 'ox-html
;;   (setq org-html-html5-fancy t
;; 	org-html-doctype "html5"
;; 	org-html-htmlize-output-type 'css)
;;   (setq org-export-global-macros
;;         '(("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@")))
;;   (setq org-html-preamble t)
;;   (setq org-html-preamble-format
;; 	'(("en" "<a href=\"/index.html\" class=\"button\">Home</a>
;;                <a href=\"/posts/index.html\" class=\"button\">Posts</a>
;;                <a href=\"/about.html\" class=\"button\">About</a>
;;                <hr>")))

;;   (setq org-html-postamble t)

;;   (setq org-html-postamble-format
;;         '(("en" "<hr><div class=\"info\"> <span class=\"created\">Created with %c on MacOS</span>
;;  <span class=\"updated\">Updated: %d</span> </div>")))

;;   (setq org-html-head-include-default-style nil)

;;   (setq org-html-head
;;         "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/style.css\" />
;;          <script src=\"js/copy.js\"></script> "))


(provide 'init-blog)
