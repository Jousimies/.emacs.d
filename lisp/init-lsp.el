;; init-lsp.el --- LSP. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package files
  :config
  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode)))

(use-package treesit
  :config
  (add-to-list 'treesit-language-source-alist
               '(python "https://github.com/tree-sitter/tree-sitter-python.git")))

(use-package eglot
  :after yasnippet
  :hook ((LaTeX-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)))

;; lsp-bridge-toggle-sdcv-helper use pinyin to search english words,
;; Disable corfu-mode to turn off cape-dabbrev temporarily.
(autoload 'lsp-bridge-toggle-sdcv-helper "lsp-bridge" "" t)
(defun my/toggle-corfu ()
  "Deactivate input method when sdcv helper enabled."
  (interactive)
  (if acm-enable-search-sdcv-words
      (corfu-mode -1)
    (corfu-mode 1)))

(advice-add 'lsp-bridge-toggle-sdcv-helper :after #'my/toggle-corfu)

;; Disable sdcv helper after exit insert state if acm-enable-search-sdcv-words is true.
(with-eval-after-load 'lsp-bridge
  (add-hook 'evil-insert-state-exit-hook (lambda ()
                                           (if acm-enable-search-sdcv-words
                                               (lsp-bridge-toggle-sdcv-helper)))))

(provide 'init-lsp)
;;; init-lsp.el ends here.
