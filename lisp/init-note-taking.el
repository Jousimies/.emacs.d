;; init-taking.el --- note taking *- lexical-binding: t; no-byte-compile: t -*-

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
  :bind (("<f9>" . ekg-capture)
         ("C-<f9>" . ekg-capture-url)
         (:map ekg-notes-mode-map
               ("q" . quit-window)))
  :config
  (setq triples-default-database-filename (expand-file-name "ekg/triples.db" my-galaxy)))

(add-to-list 'display-buffer-alist '("\\*EKG"
                                     (display-buffer-pop-up-frame)
                                     (window-parameters
                                      (no-other-window . t)
                                      (mode-line-format . none)
                                      (no-delete-other-windows . t))))

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
  :commands (denote-signature denote-subdirectory denote-rename-file-using-front-matter
                              denote-rename-file
                              denote-link-or-create)
  :hook (dired-mode . denote-dired-mode)
  :config
  (setq denote-directory (expand-file-name "denote" my-galaxy)))

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global
    "gnbl" 'denote-org-dblock-insert-links
    "gnbb" 'denote-org-dblock-insert-backlinks
    "gns" 'denote-signature
    "gnd" 'denote-subdirectory
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
          ("Book Reading"  ?b ,(expand-file-name "denote/books" my-galaxy)))))
(defun my/new-article (article)
    (interactive "sTitle: ")
    (let ((filename (format "%s" article))
          (ext ".org"))
      (find-file (concat my-galaxy "/articles/" filename ext))
      (insert "#+TITLE: " article "\n")
      (tempel-insert 'hugo)))

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global
    "gnn" 'consult-notes
    "gna" 'my/new-article))

;; (use-package consult-notes-org-roam
;;   :commands consult-notes-org-roam-find-node-relation)

;; (my/space-leader-def
;;     "nv" '(consult-notes-org-roam-find-node-relation :wk "Node navigation"))

(use-package org-transclusion
  :commands (org-transclusion-make-from-link org-transclusion-add org-transclusion-add-all)
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
(my/space-leader-def
  "t" '(:ignore t :wk "Transclusion")
  "ta" '(org-transclusion-add :wk "Add")
  "tA" '(org-transclusion-add-all :wk "Add all")
  "tr" '(org-transclusion-remove :wk "Remove")
  "tR" '(org-transclusion-remove-all :wk "Remove all")
  "tg" '(org-transclusion-refresh :wk "Refresh")
  "tm" '(org-transclusion-make-from-link :wk "Make link")
  "to" '(org-transclusion-open-source :wk "Open source")
  "te" '(org-transclusion-live-sync-start :wk "Edit live"))

(provide 'init-note-taking)
;;; init-note-taking.el ends here.
