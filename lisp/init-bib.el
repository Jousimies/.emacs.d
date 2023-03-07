;; init-bib.el --- bib management *- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package oc
  :after org
  :config
  (setq org-cite-global-bibliography `(,(concat my-galaxy "/bibtexs/References.bib"))))

(use-package citar
  :commands citar-open citar-open-entry citar-open-files citar-open-notes citar-open-links
  :config
  (setq citar-bibliography org-cite-global-bibliography)
  (setq citar-notes-paths `(,(expand-file-name "references" my-galaxy)))
  (setq citar-library-file-extensions '("pdf" "jpg" "epub"))
  (setq citar-templates '((main . "${author editor:30} ${date year issued:4} ${title:48}")
                          (suffix . "${=key= id:15} ${=type=:12} ${tags keywords:*}")
                          (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
                          (note . "${title}")))
  (setq citar-symbol-separator "  ")
  (setq citar-file-additional-files-separator "-")
  (setq citar-at-point-function 'embark-act)
  (with-eval-after-load 'all-the-icons
   (setq citar-symbols
        `((file ,(all-the-icons-faicon "file-pdf-o" :face 'all-the-icons-dred :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))))

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
  :commands citar-embark-mode
  :hook (org-mode . citar-embark-mode))

(my/space-leader-def
  "re" '(citar-open-entry :wk "Open entry")
  "rp" '(citar-open-files :wk "Open files")
  "ri" '(citar-insert-citation :wk "Insert citation")
  "rn" '(citar-open-notes :wk "Open/Create note")
  "rl" '(citar-open-links :wk "Open links"))

(use-package citar-denote
  :after citar
  :config
  (defun my/citar-open-notes (citekeys)
    "Open notes associated with the CITEKEYS."
    (interactive (list (citar-select-refs :filter (citar-denote-has-notes))))
    (pcase (let ((embark-default-action-overrides
                  (cons (cons t #'citar--open-resource)
                        (bound-and-true-p embark-default-action-overrides))))
             (citar--select-resource citekeys :notes t :create-notes t))
      (`(note . ,note) (citar-open-note note))
      (`(create-note . ,citekey) (citar-create-note citekey))))
  (advice-add 'citar-denote-open-note :override 'my/citar-open-notes))

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

  (setq ebib-keywords (concat org-roam-directory "/bibtexs/keywords.txt"))
  (setq ebib-notes-directory (concat org-roam-directory "/ref"))
  (setq ebib-filters-default-file (concat org-roam-directory "/bibtexs/ebib-filters"))
  (setq ebib-reading-list-file (concat org-roam-directory "/bibtexs/reading_list.org"))

  (setq ebib-keywords-field-keep-sorted t)
  (setq ebib-keywords-file-save-on-exit 'always)

  (setq ebib-index-columns
        '(("Entry Key" 30 t) ("Note" 1 nil) ("Year" 6 t) ("Title" 50 t)))
  (setq ebib-file-associations '(("ps" . "gv"))))

(use-package scihub
  :commands scihub
  :config
  (setq scihub-download-directory "~/Downloads/")
  (setq scihub-open-after-download t)
  (setq scihub-fetch-domain 'scihub-fetch-domains-lovescihub))

(use-package biblio
  :commands biblio-lookup
  :general
  (:states 'normal
           "SPC rd" 'my/biblio-lookup-crossref :no-autoload t)
  :preface
  (defun my/biblio-lookup-crossref ()
    (interactive)
    (biblio-lookup 'biblio-crossref-backend)))

(my/space-leader-def
  "rd" '(my/biblio-lookup-crossref :wk "Get bib from crossfer"))

(provide 'init-bib)
;;; init-bib.el ends here.
