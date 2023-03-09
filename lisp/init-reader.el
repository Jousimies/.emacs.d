;; init-reader.el --- reader, pdf-tools *- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package pdf-tools
  :hook ((doc-view-mode . pdf-tools-install)
         (dirvish-setup . pdf-tools-install)
         (pdf-tools-enabled . pdf-view-themed-minor-mode)))

(use-package pdf-view
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install t nil t nil)
  (evil-declare-key 'normal pdf-view-mode-map
    "gh" 'pdf-annot-add-highlight-markup-annotation
    "ga" 'pdf-annot-add-text-annotation
    "gd" 'pdf-annot-delete)
  :init
  (setq pdf-view-display-size 'fit-width)
  (setq pdf-view-use-unicode-ligther nil)
  (setq pdf-view-use-scaling t)
  (setq pdf-view-use-imagemagick nil)
  (setq pdf-annot-activate-created-annotations nil))

(add-to-list 'display-buffer-alist '("\\.pdf"
                                     (display-buffer-in-tab)
                                     (tab-name . "PDF")
                                     (tab-group . "PDF")))
(use-package pdf-occur
  :hook (pdf-view-mode . pdf-occur-global-minor-mode))

(use-package pdf-history
  :hook (pdf-view-mode . pdf-history-minor-mode))

(use-package pdf-links
  :hook (pdf-view-mode . pdf-links-minor-mode))

(defun my/get-file-name ()
  "Copy pdf file name."
  (interactive)
  (kill-new (file-name-base (buffer-file-name)))
  (message "Copied %s" (file-name-base (buffer-file-name))))

(use-package pdf-outline
  :hook (pdf-view-mode . pdf-outline-minor-mode)
  :bind (:map pdf-outline-buffer-mode-map
              ("RET" . pdf-outline-follow-link-and-quit)))

(add-to-list 'display-buffer-alist '("\\*Outline"
                                     (display-buffer-in-side-window)
                                     (side . right)
                                     (window-width . 0.5)
                                     (window-parameters
                                      (mode-line-format . none))))
(use-package pdf-annot
  :hook (pdf-view-mode . pdf-annot-minor-mode)
  :bind (:map pdf-annot-edit-contents-minor-mode-map
              ("<return>" . pdf-annot-edit-contents-commit)
              ("<S-return>" . newline)))

(use-package pdf-sync
  :hook (pdf-view-mode . pdf-sync-minor-mode))

(use-package pdf-cache
  :after pdf-view
  :config
  (define-pdf-cache-function pagelabels))

(use-package pdf-misc
  :after pdf-view
  :config
  (setq pdf-misc-print-program-executable "/usr/bin/lp")

  (defun mrb/pdf-misc-print-pages(filename pages &optional interactive-p)
    "Wrapper for `pdf-misc-print-document` to add page selection support."
    (interactive (list (pdf-view-buffer-file-name)
                       (read-string "Page range (empty for all pages): "
                                    (number-to-string (pdf-view-current-page)))
                       t) pdf-view-mode)
    (let ((pdf-misc-print-program-args
           (if (not (string-blank-p pages))
               (cons (concat "-P " pages) pdf-misc-print-program-args)
             pdf-misc-print-program-args)))
      (pdf-misc-print-document filename)))
  :bind (:map pdf-view-mode-map
              ([remap pdf-misc-print-document] . mrb/pdf-misc-print-pages)))

(defun my/pdf-extract-highlight ()
  "Extract highlight to plain text.
When it finised, it will jump to note file."
  (interactive)
  (let* ((pdf-filename (buffer-name))
         (txt-filename (make-temp-file "/tmp/annot-"))
         (org-file (read-file-name "Save extracted highlights to org file: " (expand-file-name "references/" my-galaxy))))
    (async-start
     `(lambda ()
        (shell-command-to-string (format "/opt/homebrew/bin/python3.10 ~/pdfannots/pdfannots.py \"%s\""
                                          ,pdf-filename)))
     `(lambda (annot)
        (setq annot (replace-regexp-in-string "##" "*" annot))
        (with-temp-buffer
          (insert annot)
          (write-region (point-min) (point-max) ,org-file t))
        (find-file ,org-file)))))

(my/space-leader-def
  "mh" '(my/pdf-extract-highlight :wk "Extract highlight"))

(defun my/dired-pdf-to-png ()
  (interactive)
  (let* ((filename (dired-get-filename)))
    (if (string-match "\.pdf" filename)
        (let* ((pdf-base-name (file-name-sans-extension (file-name-nondirectory filename)))
               (png (concat pdf-base-name ".png"))
               (pdf-info (shell-command-to-string (format "pdfinfo %s | grep Pages | awk '{print $2}'" filename)))
               (pdf-pages (string-to-number pdf-info)))
          (when (file-exists-p png)
            (delete-file png))
          (if (= pdf-pages 1)
              (start-process-shell-command
               "pdf-to-png"
               nil
               (format "pdftoppm -singlefile -r 600 %s %s -png" filename pdf-base-name))
            (start-process-shell-command
             "pdf-to-png"
             nil
             (format "pdftoppm -r 600 %s %s -png" filename pdf-base-name))))
      (message "Current file is not a PDF file."))))

(use-package saveplace-pdf-view
  :after pdf-tools
  :config
  (save-place-mode))

(use-package nov
  :mode (".epub" . nov-mode)
  :config
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
