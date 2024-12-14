;; init-latex.el --- Latex editor. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/packages/citeproc-el/")
(add-to-list 'load-path "~/.emacs.d/packages/queue/")
;; ox-latex 是 Emacs 中 Org-mode 导出框架中的一个子模块
;; minted 需要安装 Pygments, brew install pygments
(with-eval-after-load 'ox-latex
  (setopt org-latex-compiler "xelatex")
  (setopt org-latex-prefer-user-labels t)
  (setopt org-latex-src-block-backend 'minted) ;file name should not contain space.
  ;; (setopt org-latex-minted-options '(("linenos")
  ;; 									 ("numbersep" "5pt")
  ;; 									 ("frame"     "none") ; box frame is created by `mdframed' package
  ;; 									 ("framesep"  "2mm")
  ;; 									 ("breaklines")))
  (setopt org-latex-pdf-process '("xelatex -shell-escape %f"
                           "bibtex -shell-escape %b"
                           "xelatex -shell-escape %f"
                           "xelatex -shell-escape %f"
                           "rm -fr %b.out %b.log %b.tex %b.brf %b.bbl"))
  (setopt org-latex-logfiles-extensions '("lof" "lot" "tex~" "aux" "idx" "log"
                                   "out" "toc" "nav" "snm" "vrb" "dvi"
                                   "fdb_latexmk" "blg" "brf" "fls"
                                   "entoc" "ps" "spl" "bbl"))
  (setopt org-latex-hyperref-template  (concat "\\hypersetup{\n"
                                        "pdfauthor={%a},\n"
                                        "pdftitle={%t},\n"
                                        "pdfkeywords={%k},\n"
                                        "pdfsubject={%d},\n"
                                        "pdfcreator={%c},\n"
                                        "pdflang={%L},\n"
                                        "colorlinks,\n"
                                        "citecolor=gray,\n"
                                        "filecolor=gray,\n"
                                        "linkcolor=gray,\n"
                                        "urlcolor=gray\n"
                                        "}\n"))
  (setopt org-latex-classes nil)
  (add-to-list 'org-latex-classes
               '("book"
                 "\\documentclass[UTF8,twoside,a4paper,12pt,openright]{ctexrep}
                   [NO-DEFAULT-PACKAGES]
                   [NO-PACKAGES]
                   [EXTRA]"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("article-cn"
                 "\\documentclass{ctexart}
                  [NO-DEFAULT-PACKAGES]
                  [NO-PACKAGES]
                  [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("article"
                 "\\documentclass[11pt]{article}
                 [NO-DEFAULT-PACKAGES]
                 [NO-PACKAGES]
                 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass[presentation]{beamer}
                 [DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;; The builtin tex-mode, But AucTeX is more powerful
;; (use-package tex-mode
;;   :mode ("\\.tex\\'" . latex-mode))

(use-package tex
  :load-path "packages/auctex/"
  :init
  (load "auctex.el" nil t t)
  (load "preview-latex.el" nil t t)
  :mode ("\\.tex\\'" . latex-mode)
  :custom
  (TeX-PDF-mode t)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-save-query nil)
  (TeX-master 'dwim)
  (TeX-electric-sub-and-superscript t)
  (TeX-auto-local (expand-file-name ".auctex-auto" cache-directory))
  (TeX-style-local (expand-file-name ".auctex-style" cache-directory))
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method '((dvi . source-specials) (pdf . synctex)))
  (TeX-source-correlate-start-server t))

(with-eval-after-load 'tex              ;auctex
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex -shell-escape --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
  (add-to-list 'TeX-view-program-list-builtin '("PDF Tools" TeX-pdf-tools-sync-view))
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "C-c h .") #'TeX-doc))

(use-package auctex-latexmk
  :load-path "packages/auctex-latexmk/"
  :hook (LaTeX-mode . auctex-latexmk-setup))

(use-package reftex
  :hook (LaTeX-mode . turn-on-reftex)
  :bind ([remap reftex-citation] . citar-insert-citation)
  :custom
  (reftex-toc-follow-mode t)
  (reftex-toc-split-windows-horizontally t)
  (reftex-toc-split-windows-fraction 0.25))

(use-package consult-reftex
  :load-path "~/.emacs.d/packages/consult-reftex/")

;; brew install typst
;; set `treesit-language-source-alist', see init-lsp.el
;; lsp, see https://myriad-dreamin.github.io/tinymist/frontend/emacs.html
(use-package typst-ts-mode
  :load-path "packages/typst-ts-mode/"
  :mode ("\\.typ\\'" . typst-ts-mode)
  :custom
  (typst-ts-mode-executable-location "/opt/homebrew/bin/typst")
  (typst-ts-mode-watch-options "--open"))

(provide 'init-latex)
;;; init-latex.el ends here.
