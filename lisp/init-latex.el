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


(provide 'init-latex)
