;; init-lsp.el --- LSP. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; Find treesit language source here:
;; https://github.com/tree-sitter/tree-sitter/wiki/List-of-parsers
(use-package treesit
  :ensure nil
  :when (treesit-available-p)
  :custom
  (treesit-language-source-alist
   '((python "https://github.com/tree-sitter/tree-sitter-python.git")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml.git")
     (typst "https://github.com/uben0/tree-sitter-typst.git"))))

(use-package eglot
  :after yasnippet
  :hook ((LaTeX-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)))

;; lsp-bridge-toggle-sdcv-helper use pinyin to search english words,
;; Disable corfu-mode to turn off cape-dabbrev temporarily.
(use-package lsp-bridge
  :load-path "packages/lsp-bridge/"
  :commands lsp-bridge-toggle-sdcv-helper
  :custom
  (lsp-bridge-python-command "~/.env/bin/python3")
  :config
  (defun my/toggle-corfu ()
    "Deactivate input method when sdcv helper enabled."
    (interactive)
    (if acm-enable-search-sdcv-words
        (corfu-mode -1)
      (corfu-mode 1)))
  (advice-add 'lsp-bridge-toggle-sdcv-helper :after #'my/toggle-corfu))


(provide 'init-lsp)
;;; init-lsp.el ends here.
