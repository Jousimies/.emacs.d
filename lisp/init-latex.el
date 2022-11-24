;;; init-latex.el --- Latex.   -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;; Need to update.
;; Some sources can been used as reference:
;; https://github.com/malb/emacs.d

;;; Code:
(with-eval-after-load 'ox-latex
    (setq org-latex-classes nil
          org-latex-listings 'minted
          org-export-latex-listings 'minted
          org-latex-minted-options '(("breaklines" "true")
                                     ("breakanywhere" "true")))
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
    (add-to-list 'org-latex-classes '("article-cn" "\\documentclass{ctexart}
                                      [NO-DEFAULT-PACKAGES]
                                      [NO-PACKAGES]
                                      [EXTRA]"
                                      ("\\section{%s}" . "\\section*{%s}")
                                      ("\\subsection{%s}" . "\\subsection*{%s}")
                                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                      ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes '("article" "\\documentclass[11pt]{article}
                                      [NO-DEFAULT-PACKAGES]
                                      [NO-PACKAGES]
                                      [EXTRA]"
                                      ("\\section{%s}" . "\\section*{%s}")
                                      ("\\subsection{%s}" . "\\subsection*{%s}")
                                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                      ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes '("beamer" "\\documentclass[presentation]{beamer}
                                      [DEFAULT-PACKAGES]
                                      [PACKAGES]
                                      [EXTRA]"
                                      ("\\section{%s}" . "\\section*{%s}")
                                      ("\\subsection{%s}" . "\\subsection*{%s}")
                                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(when (maybe-require-package 'auctex)
  (add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  (setq TeX-save-query nil)
  (setq TeX-electric-sub-and-superscript t)
  (setq TeX-auto-local ".auctex-auto")
  (setq TeX-style-local ".auctex-style")
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server nil)


  (setq-default TeX-master nil)

  (setq LaTeX-section-hook '(LaTeX-section-heading
                             LaTeX-section-title
                             LaTeX-section-toc
                             LaTeX-section-section
                             LaTeX-section-label))
  (setq LaTeX-fill-break-at-separators nil)
  (setq LaTeX-item-indent 0)

  (setq org-highlight-latex-and-related '(latex script))

  (setq org-latex-pdf-process '("xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"
                                "bibtex -shell-escape %b"
                                "xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"
                                "xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"
                                "rm -fr %b.out %b.log %b.tex %b.brf %b.bbl")
        org-latex-logfiles-extensions '("lof" "lot" "tex~" "aux" "idx" "log"
                                        "out" "toc" "nav" "snm" "vrb" "dvi"
                                        "fdb_latexmk" "blg" "brf" "fls"
                                        "entoc" "ps" "spl" "bbl")
        org-latex-prefer-user-labels t)

  ;; Beamer
  (add-hook 'org-mode-hook #'(lambda ()
                               (require 'ox-beamer)))

  ;; Refter
  (when (maybe-require-package 'reftex)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (add-hook 'latex-mode-hook 'turn-on-reftex)
    (setq reftex-toc-split-windows-horizontally t)
    (setq reftex-toc-split-windows-fraction 0.25)
    (add-hook 'reftex-toc-mode-hook 'menu-bar--visual-line-mode-enable)
    (add-hook 'reftex-toc-mode-hook #'(lambda () (setq-local mode-line-format nil)))))


(provide 'init-latex)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-latex.el ends here
