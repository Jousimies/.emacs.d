;; init-shell.el --- Shell. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package vterm
  :load-path "packages/emacs-libvterm/"
  :bind ("<f5>" . vterm)
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000))

(provide 'init-shell)
;;; init-shell.el ends here.
