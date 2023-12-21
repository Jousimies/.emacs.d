;; init-bib.el --- Bibtex management. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/packages/parsebib/")

(defvar my/reference-lists `(,(concat my-galaxy "/bibtexs/References.bib")
                             ,(concat my-galaxy "/bibtexs/Books.bib")))

(with-eval-after-load 'oc
  (setq org-cite-global-bibliography my/reference-lists)
  (require 'citar-org)
  (with-eval-after-load 'citar
	(setq org-cite-insert-processor 'citar)
	(setq org-cite-follow-processor 'citar)
	(setq org-cite-activate-processor 'citar)))

(use-package bibtex
  :mode ("\\.bib\\'" . bibtex-mode)
  :config
  (setq bibtex-align-at-equal-sign t)
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5))

(use-package ebib
  :load-path "packages/ebib/"
  :bind ("<f2>" . ebib)
  :config
  (setq ebib-preload-bib-files my/reference-lists)
  (setq ebib-keywords (concat my-galaxy "/bibtexs/keywords.txt"))
  (setq ebib-notes-directory (concat my-galaxy "/references"))
  (setq ebib-filters-default-file (concat my-galaxy "/bibtexs/ebib-filters"))
  (setq ebib-reading-list-file (concat my-galaxy "/bibtexs/reading_list.org"))
  (setq ebib-keywords-field-keep-sorted t)
  (setq ebib-keywords-file-save-on-exit 'always)
  (setq ebib-index-columns
        '(("Entry Key" 30 t) ("Note" 1 nil) ("Year" 6 t) ("Title" 50 t)))
  (setq ebib-file-associations '(("ps" . "gv"))))

(use-package citar
  :load-path "packages/citar/"
  :commands citar-open-files citar-open citar-create-note
  :config
  (setq citar-indicator-files
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-file_o"
              :face 'nerd-icons-green
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  " ; need this because the default padding is too low for these icons
     :tag "has:files"))
  (setq citar-indicator-links
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-link"
              :face 'nerd-icons-orange
              :v-adjust 0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))
  (setq citar-indicator-notes
    (citar-indicator-create
     :symbol (nerd-icons-codicon
              "nf-cod-note"
              :face 'nerd-icons-blue
              :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "    "
     :tag "has:notes"))
  (setq citar-indicator-cited
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-circle_o"
              :face 'nerd-icon-green)
     :function #'citar-is-cited
     :padding "  "
     :tag "is:cited"))
  (setq citar-indicators (list citar-indicator-links
                               citar-indicator-files
                               citar-indicator-notes
                               citar-indicator-cited))
  (setopt citar-templates '((main . "${author editor:40%sn} | ${date year issued:4} | ${title:110}")
							(suffix . "​​​​${=key= id:15} ${=type=:12}")
							(preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.")
							(note . "Notes on ${author editor:%etal}, ${title}")))
  (add-hook 'minibuffer-setup-hook
            (lambda () (setq truncate-lines t)))
  (setq citar-bibliography my/reference-lists)
  (setq citar-notes-paths `(,(expand-file-name "denote/references" my-galaxy)))
  (setq citar-library-file-extensions '("pdf" "jpg" "epub"))
  (setq citar-symbol-separator "​")
  (setq citar-file-additional-files-separator "-")
  (setq citar-at-point-function 'embark-act))

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

(provide 'init-bib)
;;; init-bib.el ends here.
