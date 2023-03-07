;; init-auto-save.el --- auto-save *- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package auto-save
  :hook (on-first-file . auto-save-enable)
  :config
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t))

(provide 'init-auto-save)
;;; init-beanchmark.el ends here.
