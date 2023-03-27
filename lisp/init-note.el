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

(evil-define-key '(normal visual) 'global
    "ged" 'ekg-show-notes-for-today
    "gee" 'ekg-show-notes-with-tag
    "gea" 'ekg-show-notes-with-any-tags
    "geA" 'ekg-show-notes-with-all-tags
    "geb" 'ekg-browse-url
    "ger" 'ekg-rename-tag)
(evil-define-key 'normal ekg-notes-mode-map
    "A" 'ekg-notes-any-tags
    "B" 'ekg-notes-select-and-browse-url
    "a" 'ekg-notes-any-note-tags
    "b" 'ekg-notes-browse
    "c" 'ekg-notes-create
    "d" 'ekg-notes-delete
    "n" 'ekg-notes-next
    "o" 'ekg-notes-open
    "p" 'ekg-notes-previous
    "r" 'ekg-notes-remove
    "t" 'ekg-notes-tag
    "q" 'quit-window)

(use-package denote
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

(evil-define-key '(normal visual motion) dired-mode-map
  "gnr" 'denote-dired-rename-marked-files)

(evil-define-key '(normal visual motion) org-mode-map
  "gnr" 'denote-rename-file-using-front-matter
  "gnl" 'denote-link-or-create)

(use-package denote-org-dblock
  :commands denote-org-dblock-insert-backlinks denote-org-dblock-insert-links)

(evil-define-key '(normal visual) org-mode-map
  "gnL" 'denote-org-dblock-insert-links
  "gnb" 'denote-org-dblock-insert-backlinks)

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

(evil-define-key '(normal visual motion) 'global
  "gns" 'my/denote-signature-or-subdirectory)

(evil-define-key '(normal visual) 'global
  "gni" 'my/org-insert-web-page-archive)

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
        `(("Articles"  ?a  ,(concat my-galaxy "/articles"))
          ("Denote Notes"  ?d ,(expand-file-name "denote" my-galaxy))
          ("Terminology"  ?t ,(expand-file-name "denote/term" my-galaxy))
          ("Book Reading"  ?b ,(expand-file-name "denote/books" my-galaxy))
          ("Outline"  ?o ,(expand-file-name "denote/outline" my-galaxy))
          ("References"  ?r ,(expand-file-name "denote/references" my-galaxy))
          ("Literature"  ?l ,(expand-file-name "denote/literature" my-galaxy)))))

(evil-define-key '(normal motion visual) 'global
    "gnn" 'consult-notes)

(defun my/new-article (article)
    (interactive "sTitle: ")
    (let ((filename (format "%s" article))
          (ext ".org"))
      (find-file (concat my-galaxy "/articles/" filename ext))
      (insert "#+TITLE: " article "\n")
      (tempel-insert 'hugo)))

(evil-define-key '(normal motion visual) 'global
  "gna" 'my/new-article)

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

(evil-define-key '(normal visual) org-mode-map
  "gntm" 'org-transclusion-make-from-link
  "gnta" 'org-transclusion-add
  "gntA" 'org-transclusion-add-all
  "gntr" 'org-transclusion-remove
  "gntR" 'org-transclusion-remove-all
  "gntg" 'org-transclusion-refresh
  "gnto" 'org-transclusion-open-source
  "gnts" 'org-transclusion-live-sync-start)

(use-package org-noter
  :after org pdf-tools
  :config
  (setq org-noter-notes-search-path `(,(expand-file-name "references" my-galaxy))))

(provide 'init-note)
;;; init-note.el ends here.
