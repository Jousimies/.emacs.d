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


(with-eval-after-load 'oc
  (setq org-cite-global-bibliography my/reference-lists))

(with-eval-after-load 'org
  (with-eval-after-load 'oc
    (define-key org-mode-map [remap org-cite-insert] #'citar-insert-citation)
    (with-eval-after-load 'citar
      (require 'citar-org)
      (setq org-cite-insert-processor 'citar)
      (setq org-cite-follow-processor 'citar))))


(provide 'init-bib)
