;;; init-calibre.el --- Book management.  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;; Export catalog need to use argument: set entry type to mixed, default is book.
;;; Code:
(when (maybe-require-package 'calibredb)
  (setq calibredb-root-dir "~/Nextcloud/L.Calibre/")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-add-delete-original-file t)
  (setq calibredb-size-show t)
  (setq calibredb-format-character-icons t)

  (setq calibredb-ref-default-bibliography (expand-file-name "calibre.bib" calibredb-root-dir))

  (global-set-key (kbd "<f1>") 'calibredb)

  (with-eval-after-load 'evil
    (evil-set-initial-state 'calibredb-search-mode 'emacs)))

(provide 'init-calibre)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-calibre.el ends here
