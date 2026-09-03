;; -*- lexical-binding: t; -*-

(setopt my/reference-lists `(,(concat my-galaxy "/bibtexs/My Library.bib")
                             ,(concat my-galaxy "/bibtexs/Books.bib")
			     ,(concat my-galaxy "/bibtexs/Seismic.bib")))

(with-eval-after-load 'bibtex
  (setopt bibtex-align-at-equal-sign t
	  bibtex-autokey-year-length 4
	  bibtex-autokey-name-year-separator "-"
	  bibtex-autokey-year-title-separator "-"
	  bibtex-autokey-titleword-separator "-"
	  bibtex-autokey-titlewords 2
	  bibtex-autokey-titlewords-stretch 1
	  bibtex-autokey-titleword-length 5))


(setq org-cite-global-bibliography my/reference-lists)

(with-eval-after-load 'org
  (with-eval-after-load 'oc
    (define-key org-mode-map [remap org-cite-insert] #'citar-insert-citation)
    (with-eval-after-load 'citar
      (require 'citar-org)
      (setq org-cite-insert-processor 'citar)
      (setq org-cite-follow-processor 'citar))))

(use-package citar
  :commands citar-create-note
  :after org
  :custom
  (citar-templates '((main . "${=type=:12}|${date year issued:4}| ${title:80}")
		     (suffix . " |${=key= id} |${tags keywords:*} |${author editor:20%sn}") ;
		     (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.")
		     (note . "Notes on ${author editor:%etal}, ${title}")))
  (citar-indicators (list citar-indicator-links
                          citar-indicator-files
                          citar-indicator-notes
                          citar-indicator-cited))
  (citar-bibliography my/reference-lists)
  (citar-library-paths `(,(expand-file-name "PDF/" my-galaxy)))
  (citar-notes-paths `(,(expand-file-name "denote/references" my-galaxy)))
  (citar-library-file-extensions '("pdf" "jpg" "epub"))
  (citar-symbol-separator "​")
  (citar-select-multiple t)
  (citar-file-additional-files-separator "-")
  (citar-at-point-function 'embark-act)
  (citar-file-open-function #'consult-file-externally))

(use-package citar-latex
  :after tex)

(use-package citar-capf
  :hook ((LaTeX-mode . citar-capf-setup)
         (org-mode . citar-capf-setup)))

(with-eval-after-load 'citar-org
    (define-key citar-org-citation-map (kbd "RET") 'org-open-at-point))

(use-package citar-embark
  :after citar
  :hook (org-mode . citar-embark-mode))

(use-package zotra
  :commands zotra-add-entry
  :config
  (setq zotra-backend 'zotra-server)
  (setq zotra-local-server-directory "~/zotra-server/"))

(use-package biblio
  :commands biblio-lookup biblio-crossref-lookup)

(use-package scihub
  :commands scihub
  :config
  (setq scihub-download-directory "~/Downloads/"
	scihub-open-after-download t
	scihub-fetch-domain 'scihub-fetch-domains-lovescihub))

;; Need install bibutils.
;; https://sourceforge.net/p/bibutils/home/Bibutils/
(when sys/macp
  (defun my/bib2end (bib-file end-file)
    "Convert BibTeX file to EndNote file."
    (interactive
     (list (read-file-name "BibTeX File: "
			   (expand-file-name "bibtexs/" my-galaxy) nil nil ".bib")
           (read-file-name "Output EndNote File: "
			   (expand-file-name "bibtexs/" my-galaxy) nil nil ".end")))
    (let* ((xml-file (make-temp-file "bib2xml" nil ".xml"))
           (bib2xml-cmd (format "bib2xml %s > %s" bib-file xml-file))
           (xml2end-cmd (format "xml2end %s > %s" xml-file end-file)))
      (when (= 0 (shell-command bib2xml-cmd))
	(shell-command xml2end-cmd)
	(message "XML to EndNote conversion successful."))
      (delete-file xml-file))))


(provide 'init-bib)
