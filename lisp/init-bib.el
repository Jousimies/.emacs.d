(use-package oc
  :after org
  :config
  (setq org-cite-global-bibliography `(,(concat my-galaxy "/bibtexs/References.bib"))))

(use-package citar
  :commands citar-open-files citar-create-note
  :config
  (setq citar-bibliography `(,(concat my-galaxy "/bibtexs/References.bib")))
  (setq citar-notes-paths `(,(expand-file-name "references" my-galaxy)))
  (setq citar-library-file-extensions '("pdf" "jpg" "epub"))
  (setq citar-templates '((main . "${author editor:30} ${date year issued:4} ${title:48}")
                          (suffix . "${=key= id:15} ${=type=:12} ${tags keywords:*}")
                          (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
                          (note . "${title}")))
  (setq citar-symbol-separator "  ")
  (setq citar-file-additional-files-separator "-")
  (setq citar-at-point-function 'embark-act))

(evil-define-key 'normal 'global
  "gnp" 'citar-open-files)

(use-package citar-latex
  :after citar)

(use-package citar-capf
  :hook ((LaTeX-mode . citar-capf-setup)
         (org-mode . citar-capf-setup)))

(use-package citar-org
  :after citar
  :config
  (setq org-cite-insert-processor 'citar)
  (setq org-cite-follow-processor 'citar)
  (setq org-cite-activate-processor 'citar)
  (with-eval-after-load 'citar-org
    (define-key citar-org-citation-map (kbd "RET") 'org-open-at-point)))

(use-package citar-embark
  :hook (org-mode . citar-embark-mode))

(use-package citar-denote
  :config
  (setq citar-denote-subdir t)
  (citar-denote-mode)

  (defun my/citar-denote-create-note (citekey &optional _entry)
    "Create a bibliography note for CITEKEY with properties ENTRY.

The file type for the new note is determined by `citar-denote-file-type'.
The title of the new note is set by `citar-denote-title-format'.
When `citar-denote-subdir' is non-nil, prompt for a subdirectory."
    (denote
     (read-string "Title: " (citar-denote-generate-title citekey))
     (citar-denote-keywords-prompt citekey)
     citar-denote-file-type
     (when citar-denote-subdir (expand-file-name "references" denote-directory)))
    (citar-denote-add-reference citekey))

  (advice-add 'citar-denote-create-note :override #'my/citar-denote-create-note))

(defun my/citar-denote-find-ref-or-citation (arg)
  (interactive "P")
  (if arg
      (citar-denote-find-citation)
    (citar-denote-find-reference)))

(evil-define-key '(normal visual motion) 'global
  "gnk" 'citar-denote-add-citekey
  "gnK" 'citar-denote-remove-citekey
  "gno" 'citar-denote-open-note
  "gnf" 'my/citar-denote-find-ref-or-citation
  "gnN" 'citar-denote-dwim)

(use-package bibtex-completion
  :after org-roam-bibtex
  :config
  (setq bibtex-completion-bibliography org-cite-global-bibliography)
  (setq bibtex-completion-notes-path (expand-file-name "roam/ref" my-galaxy))
  (setq bibtex-completion-pdf-field "File")
  (setq bibtex-completion-additional-search-fields '(keywords journal booktitle))
  (setq bibtex-completion-pdf-symbol "P")
  (setq bibtex-completion-notes-symbol "N")
  (setq bibtex-completion-display-formats '((article . "${=has-pdf=:1} ${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
                                            (inbook . "${=has-pdf=:1} ${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
                                            (incollection . "${=has-pdf=:1} ${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
                                            (inproceedings . "${=has-pdf=:1} ${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
                                            (t . "${=has-pdf=:1} ${=has-note=:1} ${year:4} ${author:36} ${title:*}"))))

(use-package ebib
  :bind ("<f2>" . ebib)
  :config
  (setq ebib-preload-bib-files org-cite-global-bibliography)

  (setq ebib-keywords (concat my-galaxy "/bibtexs/keywords.txt"))
  (setq ebib-notes-directory (concat my-galaxy "/references"))
  (setq ebib-filters-default-file (concat my-galaxy "/bibtexs/ebib-filters"))
  (setq ebib-reading-list-file (concat my-galaxy "/bibtexs/reading_list.org"))

  (setq ebib-keywords-field-keep-sorted t)
  (setq ebib-keywords-file-save-on-exit 'always)

  (setq ebib-index-columns
        '(("Entry Key" 30 t) ("Note" 1 nil) ("Year" 6 t) ("Title" 50 t)))
  (setq ebib-file-associations '(("ps" . "gv"))))

(use-package biblio
  :commands my/biblio-lookup-crossref
  :config
  (defun my/biblio-lookup-crossref ()
    (interactive)
    (biblio-lookup 'biblio-crossref-backend)))

(evil-define-key '(normal visual) 'global
  "gnc" 'my/biblio-lookup-crossref)

(provide 'init-bib)
;;; init-bib.el ends here.
