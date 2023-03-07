;; init-undo.el --- undo system*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package undo-fu)

(use-package undo-fu-session
  :after undo-fu
  :hook (on-first-file . undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-directory (expand-file-name "cache/undo-fu-session" user-emacs-directory)))

(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  :bind ("C-x u" . vundo))

(provide 'init-undo)
;;; init-undo.el ends here.
