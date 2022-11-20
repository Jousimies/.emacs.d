;;; init-bibtex.el --- Bibtex management. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(setq org-cite-global-bibliography `(,(concat my-galaxy "/bibtexs/References.bib")
                                     ,(expand-file-name "L.Calibre/calibre.bib" my-cloud)))

;; bibtex-completion
(when (maybe-require-package 'bibtex-completion)
  (setq bibtex-completion-bibliography `(,(concat my-galaxy "/bibtexs/References.bib")
                                         ,(expand-file-name "L.Calibre/calibre.bib" my-cloud)))
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
;; Citar
(when (maybe-require-package 'citar)
  (setq citar-bibliography org-cite-global-bibliography)
  (setq citar-notes-paths `(,(expand-file-name "roam/ref" my-galaxy)))
  (setq citar-at-point-function 'embark-act)
  (setq citar-templates '((main . "${author editor:30} ${date year issued:4} ${title:48}")
                          (suffix . "${=key= id:15} ${=type=:12} ${tags keywords:*}")
                          (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
                          (note . "${title}")))
  (setq citar-symbol-separator "  ")
  (setq citar-library-file-extensions (list "pdf" "jpg"))
  (setq citar-file-additional-files-separator "-")

  (with-eval-after-load 'all-the-icons
    (setq citar-symbols
          `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
            (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
            (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " "))))

  ;; https://blog.tecosaur.com/tmio/2021-07-31-citations.html
  (with-eval-after-load 'citar
   (setq org-cite-insert-processor 'citar)
   (setq org-cite-follow-processor 'citar)
   (setq org-cite-activate-processor 'citar))

  (with-eval-after-load 'citar-org
    (define-key citar-org-citation-map (kbd "<return>") 'org-open-at-point)
    (define-key org-mode-map (kbd "C-c C-x @") 'citar-insert-citation))

  ;; citar-org-roam
  (when (maybe-require-package 'citar-org-roam)
    (citar-org-roam-mode)
    (with-eval-after-load 'citar-org-roam
      (setq citar-org-roam-subdir "ref")
      (setq citar-org-roam-note-title-template "${title}"))

    ;; Temporarily work, wait citar-org-roam update to support capture with template.
    (defun my/citar-org-roam--create-capture-note (citekey entry)
     "Open or create org-roam node for CITEKEY and ENTRY."
   ;; adapted from https://jethrokuan.github.io/org-roam-guide/#orgc48eb0d
     (let ((title (citar-format--entry
                    citar-org-roam-note-title-template entry)))
      (org-roam-capture-
       :templates
       '(("r" "reference" plain (file "~/.emacs.d/template/reference") :if-new ;; Change "%?" to a template file.
          (file+head
           "%(concat
              (when citar-org-roam-subdir (concat citar-org-roam-subdir \"/\")) \"${citekey}.org\")"
           "#+title: ${title}\n")
          :immediate-finish t
          :unnarrowed t))
       :info (list :citekey citekey)
       :node (org-roam-node-create :title title)
       :props '(:finalize find-file))
      (org-roam-ref-add (concat "@" citekey))))
    (advice-add 'citar-org-roam--create-capture-note :override #'my/citar-org-roam--create-capture-note))

  (when (maybe-require-package 'citar-embark)
    (add-hook 'after-init-hook 'citar-embark-mode))

  (with-eval-after-load 'org-roam
    (require-package 'org-roam-bibtex))

  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "nb" '(:ignore t :wk "Bibtex")
   "nbc" '(citar-org-roam-cited :wk "Cited Roam Node")
   "nbf" '(citar-open-files :wk "Open files")
   "nbe" '(citar-open-entry :wk "Open entry")
   "nbn" '(citar-open-notes :wk "Open/Create note")
   "nbl" '(citar-open-links :wk "Open links"))

  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   :keymaps 'org-mode-map
   "nbm" '(orb-note-actions :wk "ORB Menu")))

;; == Can do, but not useful.
;; use biblio to search bibtex.
;; 不怎么使用这个功能，Zotero 在这个方面更好使。
(require-package 'biblio)

;; 常出错，不如使用网页版进行。
(when (maybe-require-package 'scihub)
  (setq scihub-download-directory "~/Downloads/")
  (setq scihub-open-after-download t))

(provide 'init-bibtex)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-bibtex.el ends here
