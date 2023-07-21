;; init-bib.el --- Bibtex management. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package oc
  :after org
  :config
  (setq org-cite-global-bibliography my/reference-lists))

(use-package citar
  :commands citar-open-files citar-create-note
  :config
  (setq citar-bibliography my/reference-lists)
  (setq citar-notes-paths `(,(expand-file-name "denote/references" my-galaxy)))
  (setq citar-library-file-extensions '("pdf" "jpg" "epub"))
  (setq citar-templates '((main . "${author editor:30} ${date year issued:4} ${title:48}")
                          (suffix . "${=key= id:15} ${=type=:12} ${tags keywords:*}")
                          (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
                          (note . "${title}")))
  (setq citar-symbol-separator "  ")
  (setq citar-file-additional-files-separator "-")
  (setq citar-at-point-function 'embark-act))

;; (use-package citar-latex
;;   :after citar)

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

  :after citar
  :hook (org-mode . citar-embark-mode))

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

(provide 'init-bib)
;;; init-bib.el ends here.
