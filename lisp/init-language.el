;; init-language.el --- lsp and programming language *- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package markdown-mode
  :mode (("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; (with-eval-after-load 'whitespace-cleanup-mode
;;   (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode))

(use-package devdocs-browser
  :commands devdocs-browser-install-doc devdocs-browser-open devdocs-browser-open-in
  :config
  (setq devdocs-browser-cache-directory (expand-file-name "cache/devdocs-browser/" user-emacs-directory)))

(add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . toml-mode))

(use-package yaml-mode
  :mode ("\\.yaml\\'" . yaml-mode))
  ;; :config
  ;; (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

(provide 'init-language)
;;; init-language.el ends here.
