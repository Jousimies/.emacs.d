;; -*- lexical-binding: t; -*-

(setopt my/reference-lists `(,(concat my-galaxy "/bibtexs/My Library.bib")
                             ,(concat my-galaxy "/bibtexs/Books.bib")
			     ,(concat my-galaxy "/bibtexs/Seismic.bib")))
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

(use-package reftex
  :hook (LaTeX-mode . turn-on-reftex)
  :bind ([remap reftex-citation] . citar-insert-citation)
  :config
  (setf (alist-get "\\*RefTex" display-buffer-alist nil t #'equal)
        '((display-buffer-in-side-window)
          (window-height . 0.25)
          (side . bottom) (slot . -9)))
  :custom
  (reftex-insert-label-flags '("sf" "sfte"))
  (reftex-plug-into-AUCTeX t)
  (reftex-ref-style-default-list '("Default" "AMSmath" "Cleveref"))
  (reftex-use-multiple-selection-buffers t)
  (reftex-default-bibliography org-cite-global-bibliography)
  (reftex-toc-follow-mode t)
  (reftex-toc-split-windows-horizontally t)
  (reftex-toc-split-windows-fraction 0.25))

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
