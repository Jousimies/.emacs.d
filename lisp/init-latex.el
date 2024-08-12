;; init-latex.el --- Latex editor. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package tex-mode
  :mode ("\\.tex\\'" . latex-mode))

;; AucTeX
(use-package tex
  :ensure auctex
  :defer 2
  :init
  (load "auctex.el" nil t t)
  :custom
  (TeX-data-directory "~/.emacs.d/packages/auctex")
  (TeX-lisp-directory TeX-data-directory)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-save-query nil)
  (TeX-electric-sub-and-superscript t)
  :config
  (setq-default TeX-master t)
  (setq-default LaTeX-command  "latex -shell-escape --synctex=1")

  (setq-default TeX-engine 'xetex)
  (with-eval-after-load 'tex-mode
    (add-to-list 'tex-compile-commands '("xelatex %f" t "%r.pdf"))
    (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
    (setq TeX-command "xelatex"))

  (setq TeX-show-compilation nil)

  (setq TeX-auto-local ".auctex-auto")
  (setq TeX-style-local ".auctex-style")

  ;; [ SyncTeX ] -- Sync (forward and inverse search) PDF with TeX/LaTeX.
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method '((dvi . source-specials) (pdf . synctex)))
  (setq TeX-source-correlate-start-server t)

  ;; View
  (add-to-list 'TeX-view-program-list-builtin '("PDF Tools" TeX-pdf-tools-sync-view))
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook #'TeX-revert-document-buffer))

(use-package latex
  :ensure nil
  :bind (:map LaTeX-mode-map
              ("C-c h ." . TeX-doc)))

(use-package auctex-latexmk
  :hook (LaTeX-mode . auctex-latexmk-setup))

(use-package reftex
  :hook (LaTeX-mode . turn-on-reftex)
  :bind ([remap reftex-citation] . citar-insert-citation)
  :custom
  (reftex-toc-follow-mode t)
  (reftex-toc-split-windows-horizontally t)
  (reftex-toc-split-windows-fraction 0.25))

(use-package cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex))

;; (use-package preview-auto
;;   :custom
;;   (preview-protect-point t)
;;   (preview-locating-previews-message nil)
;;   (preview-leave-open-previews-visible t)
;;   :hook (LaTeX-mode . preview-auto-mode))

(provide 'init-latex)
;;; init-latex.el ends here.
