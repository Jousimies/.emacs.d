;;; init-bibtex.el --- Bibtex management. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(setq org-cite-global-bibliography `(,(concat my-galaxy "/bibtexs/References.bib")
                                     ,(expand-file-name "L.Calibre/calibre.bib" my-cloud)))
;; https://blog.tecosaur.com/tmio/2021-07-31-citations.html
(with-eval-after-load 'citar
  (setq org-cite-insert-processor 'citar)
  (setq org-cite-follow-processor 'citar)
  (setq org-cite-activate-processor 'citar))

(when (maybe-require-package 'citar)
  (setq citar-bibliography org-cite-global-bibliography)
  (setq citar-notes-paths `(,(expand-file-name "roam/ref" my-galaxy)))
  (setq citar-at-point-function 'embark-act)
  (setq citar-templates '((main . "${author editor:30} ${date year issued:4} ${title:48}")
                          (suffix . "${=key= id:15} ${=type=:12} ${tags keywords:*}")
                          (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
                          (note . "${title}")))
  (with-eval-after-load 'all-the-icons
    (setq citar-symbols
          `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
            (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
            (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " "))))
  (setq citar-symbol-separator "  ")
  (setq citar-open-note-function 'orb-citar-edit-note)
  (setq citar-library-file-extensions (list "pdf" "jpg"))
  (setq citar-file-additional-files-separator "-")

  (with-eval-after-load 'citar-org
    (define-key citar-org-citation-map (kbd "<return>") 'org-open-at-point)
    (define-key org-mode-map (kbd "C-c C-x @") 'citar-insert-citation))

  (with-eval-after-load 'embark
    (citar-embark-mode 1))

  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "bo" '(citar-open-files :wk "Open bibtex")
   "bO" '(citar-open-entry :wk "Show entry")
   "bn" '(citar-open-note :wk "Open note")
   "bl" '(citar-open-links :wk "Open links")))

(provide 'init-bibtex)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-bibtex.el ends here
