;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'markdown-mode)
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

  (when (maybe-require-package 'whitespace-cleanup-mode)
    (with-eval-after-load 'whitespace-cleanup-mode
      (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode))))


(provide 'init-markdown)
;;; init-markdown.el ends here
