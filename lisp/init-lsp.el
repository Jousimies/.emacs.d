;; init-lsp.el --- LSP. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package treesit
  :when (treesit-available-p)
  :commands treesit-install-language-grammar
  :custom
  (treesit-language-source-alist
   '((python "https://github.com/tree-sitter/tree-sitter-python.git")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml.git"))))

(use-package eglot
  :after yasnippet
  :hook ((LaTeX-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)))

(provide 'init-lsp)
;;; init-lsp.el ends here.
