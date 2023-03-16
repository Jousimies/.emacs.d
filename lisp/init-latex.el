(use-package tex
  :mode (".tex" . LaTeX-mode)
  :init
  (load "auctex.el" nil t t)
  (load "preview-latex.el" nil t t)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  (setq TeX-electric-sub-and-superscript t)
  (setq TeX-auto-local ".auctex-auto")
  (setq TeX-style-local ".auctex-style")
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server nil)

  (setq-default TeX-master t)
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
  (add-to-list 'TeX-view-program-list '("PDF Tools" TeX-pdf-tools-sync-view))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package latex
  :bind (:map LaTeX-mode-map
              ("C-c h" . TeX-doc)))

(use-package auctex-latexmk
  :hook (LaTeX-mode . auctex-latexmk-setup))

(use-package reftex
  :hook ((LaTeX-mode . turn-on-reftex)
         (reftex-toc-mode . menu-bar--visual-line-mode-enable))
  :config
  (setq reftex-toc-split-windows-horizontally t)
  (setq reftex-toc-split-windows-fraction 0.25))

(use-package cdlatex
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (org-mode . org-cdlatex-mode)))

(use-package ox-latex
  :after org
  :config
  (setq org-latex-src-block-backend 'minted)
  (setq org-latex-minted-options '(("breaklines" "true")
                                   ("breakanywhere" "true")))
  (setq org-latex-classes nil)
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
                                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  (setq org-latex-pdf-process
        '("xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"
          "bibtex -shell-escape %b"
          "xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"
          "xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"
          "rm -fr %b.out %b.log %b.tex %b.brf %b.bbl"))

  (setq org-latex-logfiles-extensions '("lof" "lot" "tex~" "aux" "idx" "log"
                                        "out" "toc" "nav" "snm" "vrb" "dvi"
                                        "fdb_latexmk" "blg" "brf" "fls"
                                        "entoc" "ps" "spl" "bbl"))

  (setq org-latex-prefer-user-labels t))

(use-package ox-beamer
  :after org)

(provide 'init-latex)
;;; init-latex.el ends here.
