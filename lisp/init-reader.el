;; init-reader.el --- Reader. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(require 'eaf)
(setq eaf-config-location (expand-file-name "cache/eaf" user-emacs-directory))
(require 'eaf-pdf-viewer)

(use-package nov
  :mode (".epub" . nov-mode)
  :config
  (setq nov-save-place-file (expand-file-name "cache/nov-places" user-emacs-directory))
  (setq nov-unzip-program (executable-find "bsdtar")
        nov-unzip-args '("-xC" directory "-f" filename)))

;; Export catalog need to use argument: set entry type to mixed, default is book.
(use-package calibredb
  :bind (("<f1>" . calibredb)
         :map calibredb-search-mode-map
         ("C-c l" . calibredb-copy-as-org-link))
  :config
  (setq calibredb-root-dir "~/Nextcloud/L.Calibre/")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-add-delete-original-file "yes")
  (setq calibredb-size-show t)
  (setq calibredb-format-character-icons t)

  (setq calibredb-ref-default-bibliography (expand-file-name "calibre.bib" calibredb-root-dir))

  (evil-set-initial-state 'calibredb-search-mode 'emacs))

(provide 'init-reader)
;;; init-reader.el ends here.
