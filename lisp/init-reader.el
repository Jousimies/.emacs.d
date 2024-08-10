;; init-reader.el --- Reader. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package pdf-tools
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :hook ((pdf-tools-enabled . pdf-view-themed-minor-mode)
		 (pdf-view-mode . pdf-occur-global-minor-mode)
		 (pdf-view-mode . pdf-history-minor-mode)
		 (pdf-view-mode . pdf-links-minor-mode)
		 (pdf-view-mode . pdf-outline-minor-mode)
		 (pdf-view-mode . pdf-annot-minor-mode)
		 (pdf-view-mode . pdf-sync-minor-mode))
  :bind ((:map pdf-view-mode-map
			   ([remap pdf-misc-print-document] . mrb/pdf-misc-print-pages))
		 (:map pdf-outline-buffer-mode-map
               ("RET" . pdf-outline-follow-link-and-quit))
		 (:map pdf-annot-edit-contents-minor-mode-map
               ("<return>" . pdf-annot-edit-contents-commit)
               ("<S-return>" . newline)))
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-view-use-unicode-ligther nil)
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil)
  (pdf-annot-activate-created-annotations nil)
  (pdf-misc-print-program-executable "/usr/bin/lp")
  :preface
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
  :config
  (pdf-tools-install t nil t nil))

(with-eval-after-load 'saveplace
  (use-package saveplace-pdf-view))

(use-package nov
  :mode (".epub" . nov-mode)
  :custom
  (nov-unzip-program (executable-find "bsdtar"))
  (nov-unzip-args '("-xC" directory "-f" filename)))

(provide 'init-reader)
;;; init-reader.el ends here.
