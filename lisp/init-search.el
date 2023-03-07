;; init-search.el --- Better search *- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package ctrlf
  :after evil
  :hook (on-first-buffer . ctrlf-mode)
  :config
  (evil-global-set-key 'normal (kbd "/") 'ctrlf-forward-default))

(use-package rg
  :hook (on-first-input . rg-enable-default-bindings)
  :config
  (setq rg-group-result t)
  (setq rg-show-columns t))

(provide 'init-search)
;;; init-search.el ends here.
