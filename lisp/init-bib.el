;; -*- lexical-binding: t; -*-

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

(with-eval-after-load 'citar
  (setq citar-templates '((main . "${=type=:12}|${date year issued:4}| ${title:80}")
                          (suffix . " |${=key= id} |${tags keywords:*} |${author editor:20%sn}")
                          (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.")
                          (note . "Notes on ${author editor:%etal}, ${title}")))
  (setq citar-indicators (list citar-indicator-links
                               citar-indicator-files
                               citar-indicator-notes
                               citar-indicator-cited))
  (setq citar-library-paths `(,(expand-file-name "PDF/" my-galaxy)))
  (setq citar-notes-paths `(,(expand-file-name "denote/References" my-galaxy)))
  (setq citar-library-file-extensions '("pdf" "jpg" "epub"))
  (setq citar-bibliography my/reference-lists)
  (setq citar-symbol-separator "​")
  (setq citar-select-multiple t)
  (setq citar-file-additional-files-separator "-")
  (setq citar-at-point-function 'embark-act)
  (setq citar-file-open-function #'consult-file-externally))

(add-hook 'LaTeX-mode-hook #'citar-capf-setup)
(add-hook 'org-mode-hook #'citar-capf-setup)

(with-eval-after-load 'citar-org
    (define-key citar-org-citation-map (kbd "RET") 'org-open-at-point))

(add-hook 'org-mode-hook #'citar-embark-mode)
(with-eval-after-load 'citar
  (with-eval-after-load 'denote
    (setq citar-denote-use-bib-keywords t)
    (setq citar-denote-subdir "References")
    (setq citar-denote-cite-includes-reference t)
    (citar-denote-mode)))

;; zotra
(with-eval-after-load 'zotra
  (setq zotra-backend 'zotra-server)
  (setq zotra-local-server-directory "~/zotra-server/"))

;; biblio
(with-eval-after-load 'biblio
  (add-to-list 'viper-emacs-state-mode-list 'biblio-selection-mode))

(with-eval-after-load 'scihub
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
