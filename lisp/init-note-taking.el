(use-package ekg
  :commands (ekg-show-notes-in-trash
             ekg-show-notes-for-today
             ekg-show-notes-with-tag
             ekg-show-notes-with-all-tags
             ekg-show-notes-with-any-tags
             ekg-show-rename-tag
             ekg-browse-url)
  :bind (("<f9>" . ekg-capture)
         ("C-<f9>" . ekg-capture-url)
         (:map ekg-notes-mode-map
               ("q" . quit-window)))
  :config
  (setq triples-default-database-filename (expand-file-name "database/triples.db" my-galaxy))
  (add-to-list 'display-buffer-alist '("^\\*ekg\\|^\\*EKG"
                                         (display-buffer-pop-up-frame)
                                         (window-parameters
                                          (mode-line-format . none)
                                          (delete-other-windows . t)))))

(with-eval-after-load 'evil
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
    "q" 'quit-window))

(use-package denote
  :commands (denote denote-signature denote-subdirectory denote-rename-file-using-front-matter
                    denote-keywords-prompt
                    denote-rename-file
                    denote-link-or-create)
  :hook (dired-mode . denote-dired-mode)
  :preface
  (cl-defun jf/denote-capture-reference (&key
                                         title
                                         url
                                         (keywords (denote-keywords-prompt))
                                         (domain "literature"))
    "Create a `denote' entry for the TITLE and URL.
TODO: Would it make sense to prompt for the domain?
"
    (denote title
            keywords
            'org
            (expand-file-name domain (denote-directory))
            nil))

  (cl-defun jf/menu--org-capture-elfeed-show (&key (entry elfeed-show-entry))
    "Create a `denote' from `elfeed' ENTRY."
    (interactive)
    (let* ((url (elfeed-entry-link entry))
           (title (elfeed-entry-title entry)))
      (jf/denote-capture-reference :url url :title title)))

  (defun jf/menu--org-capture-safari ()
    "Create an `denote' entry from Safari page."
    (interactive)
    (let* ((link-title-pair (grab-mac-link-safari-1))
           (url (car link-title-pair))
           (title (cadr link-title-pair)))
      (jf/denote-capture-reference :url url :title title)))
  :config
  (setq denote-directory (expand-file-name "denote" my-galaxy)))

(defvar prot-dired--limit-hist '()
  "Minibuffer history for `prot-dired-limit-regexp'.")
;;;###autoload
(defun prot-dired-limit-regexp (regexp omit)
  "Limit Dired to keep files matching REGEXP.

With optional OMIT argument as a prefix (\\[universal-argument]),
exclude files matching REGEXP.

Restore the buffer with \\<dired-mode-map>`\\[revert-buffer]'."
  (interactive
   (list
    (read-regexp
     (concat "Files "
             (when current-prefix-arg
               (propertize "NOT " 'face 'warning))
             "matching PATTERN: ")
     nil 'prot-dired--limit-hist)
    current-prefix-arg))
  (dired-mark-files-regexp regexp)
  (unless omit (dired-toggle-marks))
  (dired-do-kill-lines))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-;") 'prot-dired-limit-regexp))

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

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global
    "gndl" 'denote-org-dblock-insert-links
    "gndb" 'denote-org-dblock-insert-backlinks
    "gns" 'denote-signature
    "gne" 'jf/menu--org-capture-elfeed-show
    "gnt" 'my/denote-term
    "gnb" 'my/denote-book
    "gno" 'my/denote-outline
    "gnc" 'jf/menu--org-capture-safari
    "gnr" 'denote-rename-file-using-front-matter
    "gnR" 'denote-rename-file
    "gnl" 'denote-link-or-create))

(use-package denote-org-dblock
  :after denote org)

(use-package consult-notes
  :commands consult-notes
  :config
  (setq consult-notes-file-dir-sources
        `(("Articles"  ?a  ,(concat my-galaxy "/articles"))
          ("Denote Notes"  ?d ,(expand-file-name "denote" my-galaxy))
          ("Terminology"  ?t ,(expand-file-name "denote/term" my-galaxy))
          ("Book Reading"  ?b ,(expand-file-name "denote/books" my-galaxy))
          ("Outline"  ?o ,(expand-file-name "denote/outline" my-galaxy))
          ("Literature"  ?l ,(expand-file-name "denote/literature" my-galaxy)))))

(defun my/new-article (article)
    (interactive "sTitle: ")
    (let ((filename (format "%s" article))
          (ext ".org"))
      (find-file (concat my-galaxy "/articles/" filename ext))
      (insert "#+TITLE: " article "\n")
      (tempel-insert 'hugo)))

(with-eval-after-load 'evil
  (evil-define-key '(normal motion visual) 'global
    "gnn" 'consult-notes
    "gna" 'my/new-article))

(use-package org-transclusion
  :commands (org-transclusion-make-from-link org-transclusion-add org-transclusion-add-all)
  :general (my/space-leader-def
             "t" '(:ignore t :wk "Transclusion")
             "ta" '(org-transclusion-add :wk "Add")
             "tA" '(org-transclusion-add-all :wk "Add all")
             "tr" '(org-transclusion-remove :wk "Remove")
             "tR" '(org-transclusion-remove-all :wk "Remove all")
             "tg" '(org-transclusion-refresh :wk "Refresh")
             "tm" '(org-transclusion-make-from-link :wk "Make link")
             "to" '(org-transclusion-open-source :wk "Open source")
             "te" '(org-transclusion-live-sync-start :wk "Edit live"))
  :config
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

(provide 'init-note-taking)
;;; init-note-taking.el ends here.
