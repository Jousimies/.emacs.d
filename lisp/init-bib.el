(use-package oc
  :after org
  :config
  (setq org-cite-global-bibliography `(,(concat my-galaxy "/bibtexs/References.bib"))))

(use-package citar
  :general (my/space-leader-def
             "re" '(citar-open-entry :wk "Open entry")
             "rp" '(citar-open-files :wk "Open files")
             "ri" '(citar-insert-citation :wk "Insert citation")
             "rn" '(citar-open-notes :wk "Open/Create note")
             "rl" '(citar-open-links :wk "Open links"))
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
  :commands (citar-denote-add-citekey
             citar-denote-remove-citekey
             citar-denote-dwim
             citar-denote-open-note
             citar-denote-find-reference
             citar-denote-find-citation
             citar-denote-cite-nocite))

(with-eval-after-load 'evil
  (evil-define-key '(normal visual motion) 'global
    "gnca" 'citar-denote-add-citekey
    "gncx" 'citar-denote-remove-citekey
    "gnco" 'citar-denote-open-note
    "gncd" 'citar-denote-dwim
    "gncr" 'citar-denote-find-reference
    "gncf" 'citar-denote-find-citation))

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

(use-package scihub
  :general (my/space-leader-def
             "rs" '(scihub :wk "scihub"))
  :config
  (setq scihub-download-directory "~/Downloads/")
  (setq scihub-open-after-download t)
  (setq scihub-fetch-domain 'scihub-fetch-domains-lovescihub))

(use-package biblio
  :general (my/space-leader-def
            "rc" '(my/biblio-lookup-crossref :wk "Get bib from crossfer"))
  :config
  (defun my/biblio-lookup-crossref ()
    (interactive)
    (biblio-lookup 'biblio-crossref-backend)))

(provide 'init-bib)
;;; init-bib.el ends here.
