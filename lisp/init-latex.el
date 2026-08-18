;; -*- lexical-binding: t; -*-

(use-package auctex
  :elpaca (auctex :repo "https://git.savannah.gnu.org/git/auctex.git" :branch "main"
		  :pre-build (("make" "elpa"))
		  :build (:not elpaca--compile-info) ;; Make will take care of this step
		  :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
		  :version (lambda (_) (require 'auctex) AUCTeX-version))
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :hook (LaTeX-mode . turn-on-reftex)
  :bind (:map LaTeX-mode-map
              ("C-c h" . TeX-doc))
  :config
  (load "latex.el" nil t t)
  (load "preview-latex.el" nil t t)
  (setq-default preview-scale 1.4
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale)))
  (setq preview-auto-cache-preamble nil)
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
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  )

(use-package cdlatex
  :hook (org-mode . turn-on-org-cdlatex))

(use-package auctex-latexmk
  :hook (LaTeX-mode . auctex-latexmk-setup))

(with-eval-after-load 'reftex
  (setopt reftex-insert-label-flags '("sf" "sfte")
	  reftex-plug-into-AUCTeX t
	  reftex-ref-style-default-list '("Default" "AMSmath" "Cleveref")
	  reftex-use-multiple-selection-buffers t
	  reftex-default-bibliography org-cite-global-bibliography
	  reftex-toc-follow-mode t
	  reftex-toc-split-windows-horizontally t
	  reftex-toc-split-windows-fraction 0.25))

(add-hook 'LaTeX-mode-hook #'tuan-on-reftex)
(keymap-set global-map "<remap> <reftex-citation>" #'citar-insert-citation)
(setf (alist-get "\\*RefTex" display-buffer-alist nil t #'equal)
        '((display-buffer-in-side-window)
          (window-height . 0.25)
          (side . bottom) (slot . -9)))

(use-package ox-latex
  :ensure nil
  :defer t
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

(provide 'init-latex)
