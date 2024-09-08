;; init-bib.el --- Bibtex management. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(defvar my/reference-lists `(,(concat my-galaxy "/bibtexs/References.bib")
                             ,(concat my-galaxy "/bibtexs/Books.bib")))

;; Builtin package
(use-package oc
  :ensure nil
  :custom
  (org-cite-global-bibliography my/reference-lists))

;; Builtin package
(use-package bibtex
  :mode ("\\.bib\\'" . bibtex-mode)
  :custom
  (bibtex-align-at-equal-sign t)
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-name-year-separator "-")
  (bibtex-autokey-year-title-separator "-")
  (bibtex-autokey-titleword-separator "-")
  (bibtex-autokey-titlewords 2)
  (bibtex-autokey-titlewords-stretch 1)
  (bibtex-autokey-titleword-length 5))

(use-package citar
  :hook ((LaTeX-mode . citar-capf-setup)
         (org-mode . citar-capf-setup)
         (minibuffer-setup . (lambda ()
                               (setq-local truncate-lines t))))
  :commands citar-create-note
  :custom
  (citar-indicators (list citar-indicator-links
                          citar-indicator-files
                          citar-indicator-notes
                          citar-indicator-cited))
  (citar-templates '((main . "${=type=:12}|${date year issued:4}| ${title:80}")
					 (suffix . " |${=key= id:15} |${tags keywords:*} |${author editor:20%sn}") ;
					 (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.")
					 (note . "Notes on ${author editor:%etal}, ${title}")))
  (citar-bibliography my/reference-lists)
  (citar-library-paths `(,(expand-file-name "PDF/" my-galaxy)))
  (citar-notes-paths `(,(expand-file-name "denote/references" my-galaxy)))
  (citar-library-file-extensions '("pdf" "jpg" "epub"))
  (citar-symbol-separator "​") ;; Zero width space
  (citar-file-additional-files-separator "-")
  (citar-at-point-function 'embark-act))

(with-eval-after-load 'org
  (with-eval-after-load 'oc
	(define-key org-mode-map [remap org-cite-insert] #'citar-insert-citation)
	(require 'citar-org)
	(define-key citar-org-citation-map (kbd "RET") 'org-open-at-point)
	(with-eval-after-load 'citar
	  (setq org-cite-insert-processor 'citar)
	  (setq org-cite-follow-processor 'citar)
	  (setq org-cite-activate-processor 'citar))))

(use-package citar-embark
  :after citar embark
  :hook (org-mode . citar-embark-mode))

;; Another zotero package
;; https://gitlab.com/fvdbeek/emacs-zotero

;; https://gist.github.com/lgatto/f54888e7f16968f853346c67b232cae0
;; Create and yank bibtex entry from a DOI
;; I use zotra to get bibtex entry.
(use-package zotra
  :commands zotra-add-entry
  :custom
  (zotra-backend 'zotra-server)
  (zotra-local-server-directory "~/zotra-server/"))

;; Another package for browsing and fetching references.
(use-package biblio)

(use-package scihub
  :custom
  (scihub-download-directory "~/Downloads/")
  (scihub-open-after-download t)
  (scihub-fetch-domain 'scihub-fetch-domains-lovescihub))

;; Need install bibutils.
;; https://sourceforge.net/p/bibutils/home/Bibutils/
;;;###autoload
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
    (delete-file xml-file)))


(provide 'init-bib)
;;; init-bib.el ends here.
