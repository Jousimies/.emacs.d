;; init-note.el.el --- Note taking. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package ekg
  :commands (ekg-show-notes-in-trash
             ekg-show-notes-for-today
             ekg-show-notes-with-tag
             ekg-show-notes-with-all-tags
             ekg-show-notes-with-any-tags
             ekg-show-rename-tag
             ekg-browse-url)
  :bind (("C-<f10>" . ekg-capture)
         (:map ekg-notes-mode-map
               ("q" . quit-window)))
  :config
  (setq triples-default-database-filename (expand-file-name "database/triples.db" my-galaxy))
  (add-to-list 'display-buffer-alist '("^\\*ekg\\|^\\*EKG"
                                       (display-buffer-pop-up-frame)
                                       (window-parameters
                                        (mode-line-format . none)
                                        (delete-other-windows . t)))))

(use-package denote
  :bind ("C-c n l" . denote-link-or-create)
  :commands (denote denote-signature denote-subdirectory denote-rename-file-using-front-matter
		    denote-keywords-prompt
		    denote-rename-file
		    denote-link-or-create)
  :hook (dired-mode . denote-dired-mode-in-directories)
  :config
  (setq denote-directory (expand-file-name "denote" my-galaxy))
  (setq denote-dired-directories (list denote-directory
				       (thread-last denote-directory (expand-file-name "books"))
				       (thread-last denote-directory (expand-file-name "outline"))
				       (thread-last denote-directory (expand-file-name "literature"))
				       (thread-last denote-directory (expand-file-name "term"))
				       (thread-last denote-directory (expand-file-name "references")))))

(use-package denote-org-dblock
  :commands denote-org-dblock-insert-backlinks denote-org-dblock-insert-links)

(defvar prot-dired--limit-hist '()
  "Minibuffer history for `prot-dired-limit-regexp'.")

(defun my/dired-denote-signature-get ()
  (let* ((file (dired-get-filename))
         (signature (denote-retrieve-filename-signature file)))
    (concat "==" signature )))

(defun prot-dired-limit-regexp (regexp omit)
  "Limit Dired to keep files matching REGEXP, default search with Signature.

With optional OMIT argument as a prefix (\\[universal-argument]),
exclude files matching REGEXP.

Restore the buffer with \\<dired-mode-map>`\\[revert-buffer]'."
  (interactive
   (list
    (read-regexp
     (concat "Files "
             (when current-prefix-arg
               (propertize "NOT " 'face 'warning))
             "matching PATTERN" (format " (default: %s)" (my/dired-denote-signature-get)) ": ")
     (my/dired-denote-signature-get)
     nil)
    current-prefix-arg))
  (dired-mark-files-regexp regexp)
  (unless omit (dired-toggle-marks))
  (dired-do-kill-lines))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-;") 'prot-dired-limit-regexp))

(defun my/menu--org-capture-safari ()
  "Create an `literature' denote entry from Safari page."
  (interactive)
  (let* ((link-title-pair (grab-mac-link-safari-1))
         (url (car link-title-pair))
         (title (cadr link-title-pair))
         (keywords (denote-keywords-prompt)))
    (denote title
            keywords
            'org
            (expand-file-name "literature" (denote-directory))
            nil)
    ;; (my/org-insert-web-page-archive)
    (my/denote-reference-heading)
    (my/link-grab)
    (forward-line -3)))

(global-set-key (kbd "M-<f10>") 'my/menu--org-capture-safari)

(cl-defun my/denote-subdirectory (subdirectory)
  (denote
   (denote-title-prompt)
   (denote-keywords-prompt)
   'org
   (expand-file-name subdirectory (denote-directory))))

;;;###autoload
(defun my/denote-term ()
  (interactive)
  (my/denote-subdirectory "term"))

;;;###autoload
(defun my/denote-book ()
  (interactive)
  (my/denote-subdirectory "books"))

;;;###autoload
(defun my/denote-outline ()
  (interactive)
  (my/denote-subdirectory "outline"))

;;;###autoload
(defun my/denote-reference-heading ()
  (interactive)
  (goto-char (point-max))
  (insert "\n* References\n"))
;;;###autoload
(defun my/denote-signature-from-filename ()
  (interactive)
  (let* ((mode (buffer-local-value 'major-mode (current-buffer)))
         (file (if (eq mode 'org-mode) (buffer-file-name) (dired-get-filename)))
         (signature (denote-retrieve-filename-signature file)))
    (if signature
        (kill-new signature))))

(advice-add 'denote-signature :before #'my/denote-signature-from-filename)

(defhydra my/hydra-denote-subdirectory (:color blue
                                               :hint nil)
          "
  Create denote in subdirectory:
"
          ("t" my/denote-term "Terminology")
          ("b" my/denote-book "Books")
          ("o" my/denote-outline "Outline")
          ("r" citar-create-note "References")
          ("q" nil))

(defun my/denote-signature-or-subdirectory (arg)
  (interactive "P")
  (if arg
      (my/hydra-denote-subdirectory/body)
    (denote-signature)))

(use-package dired-x
  :bind (:map dired-mode-map
              ("C-c v" . my/denote-signature-buffer))
  :config
  (defun my/denote-signature-buffer ()
    (interactive)
    (switch-to-buffer "*denote-signatures*")
    (read-only-mode -1)
    (erase-buffer)
    (insert
     (shell-command-to-string
      "ls -l | awk /==/ | sed  's/--/=@/3' | sort -t '=' -Vk 3,3 | sed 's/=@/--/' "))
    (dired-virtual denote-directory)
    (denote-dired-mode)
    (auto-revert-mode -1)))

(use-package consult-notes
  :commands consult-notes
  :config
  (setq consult-notes-file-dir-sources
        `(("Articles"  ?a  ,(concat my-galaxy "/blogs_source/posts"))
          ("Denote Notes"  ?d ,(expand-file-name "denote" my-galaxy))
          ("Terminology"  ?t ,(expand-file-name "denote/term" my-galaxy))
          ("Book Reading"  ?b ,(expand-file-name "denote/books" my-galaxy))
          ("Outline"  ?o ,(expand-file-name "denote/outline" my-galaxy))
          ("Meet"  ?m ,(expand-file-name "meeting" my-galaxy))
          ("References"  ?r ,(expand-file-name "denote/references" my-galaxy))
          ("Literature"  ?l ,(expand-file-name "denote/literature" my-galaxy)))))

(defun my/new-article (article)
    (interactive "sTitle: ")
    (let ((filename (format "%s" article))
          (ext ".org"))
      (find-file (concat my-galaxy "/blogs_source/posts/" filename ext))
      (insert "#+TITLE: " article "\n")
      (tempel-insert 'blog)))

(defun my/new-meeting (meet)
  (interactive "sTitle: ")
  (let ((filename (format "%s-%s" (format-time-string "%Y%m%d") meet))
        (ext ".org"))
    (find-file (concat my-galaxy "/meeting/" filename ext))
    (insert "#+TITLE: " meet "\n")
    (tempel-insert 'meeting)))

(use-package denote-menu
  :commands denote-menu-list-notes)

(use-package org-transclusion
  :commands (org-transclusion-make-from-link
             org-transclusion-add
             org-transclusion-add-all
             org-transclusion-remove
             org-transclusion-remove-all
             org-transclusion-refresh
             org-transclusion-open-source
             org-transclusion-live-sync-start)
  :config
  ;; https://github.com/nobiot/org-transclusion/issues/160#issuecomment-1377714791
  (defun denote-org-transclusion-add (link plist)
    (when (string= "denote" (org-element-property :type link))
      (let* ((denote-id (org-element-property :path link))
             (file-path (denote-get-path-by-id denote-id))
             (new-link (with-temp-buffer
                         (insert "file:")
                         (insert file-path)
                         (beginning-of-buffer)
                         (org-element-link-parser))))
        (org-transclusion-add-org-file new-link plist))))
  (cl-pushnew 'denote-org-transclusion-add org-transclusion-add-functions)

  (psearch-patch org-transclusion-make-from-link
    (psearch-replace '`(string= type "id")
                     '`(string= type "denote")))

  (face-spec-set 'org-transclusion-fringe
                 '((((background light))
                    :foreground "black")
                   (t
                    :foreground "white"))
                 'face-override-spec)
  (face-spec-set 'org-transclusion-source-fringe
                 '((((background light))
                    :foreground "black")
                   (t
                    :foreground "white"))
                 'face-override-spec))

(use-package org-roam
  :commands org-roam-node-find
  :init
  (setq org-roam-directory (file-truename (expand-file-name "roam" my-galaxy)))
  :hook ((org-mode . org-roam-db-autosync-mode)
         (org-mode . (lambda () (setq-local time-stamp-active t
                                       time-stamp-start "#\\+MODIFIED:[ \t]*"
                                       time-stamp-end "$"
                                       time-stamp-format "\[%Y-%m-%d %3a %H:%M\]")
                       (add-hook 'before-save-hook 'time-stamp nil 'local))))
  :config
  (setq org-roam-database-connector 'sqlite)
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))

  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (format "(%s)" (car (split-string dirs "/")))
      ""))

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                                  :from links
                                  :where (= dest $s1)
                                  :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count)))

  (cl-defmethod org-roam-node-doom-filetitle ((node org-roam-node))
    "Return the value of \"#+title:\" (if any) from file that NODE resides in.
           If there's no file-level title in the file, return empty string."
    (or (if (= (org-roam-node-level node) 0)
            (org-roam-node-title node)
          (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
        ""))

  (cl-defmethod org-roam-node-doom-hierarchy ((node org-roam-node))
    "Return hierarchy for NODE, constructed of its file title, OLP and direct title.
           If some elements are missing, they will be stripped out."
    (let ((title     (org-roam-node-title node))
          (olp       (org-roam-node-olp   node))
          (level     (org-roam-node-level node))
          (filetitle (org-roam-node-doom-filetitle node))
          (separator (propertize " > " 'face 'shadow)))
      (cl-case level
        ;; node is a top-level file
        (0 filetitle)
        ;; node is a level 1 heading
        (1 (concat (propertize filetitle 'face '(shadow italic))
                   separator title))
        ;; node is a heading with an arbitrary outline path
        (t (concat (propertize filetitle 'face '(shadow italic))
                   separator (propertize (string-join olp " > ") 'face '(shadow italic))
                   separator title)))))

  ;; 获得文件的修改时间.
  (cl-defmethod org-roam-node-date ((node org-roam-node))
    (format-time-string "%Y-%m-%d" (org-roam-node-file-mtime node)))

  (setq org-roam-node-display-template
        (concat "${type:4} ${backlinkscount:3} "
                (propertize "${doom-hierarchy:*}" 'face 'org-level-3)
                (propertize "${tags:20}" 'face 'org-tag)
                " ")))

(provide 'init-note)
;;; init-note.el ends here.
