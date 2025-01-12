;; init-shell.el --- Shell. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package vterm
  :straight t
  :bind ("<f5>" . vterm)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 5000))

(global-set-key (kbd "C-<f5>") #'eshell)
(with-eval-after-load 'eshell
  (setopt eshell-directory-name (expand-file-name "eshell" cache-directory)))

(provide 'init-shell)
;;; init-shell.el ends here.
