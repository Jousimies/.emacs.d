;; init-reader.el --- Reader. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package pdf-tools
  :load-path ("packages/pdf-tools/lisp" "packages/tablist")
  :commands pdf-tools-install
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install t nil t nil))

(use-package pdf-view
  :hook ((pdf-tools-enabled . pdf-view-themed-minor-mode)
         (pdf-view-mode . (lambda ()
                           (require 'saveplace-pdf-view))))
  :config
  (setq pdf-view-display-size 'fit-width)
  (setq pdf-view-use-unicode-ligther nil)
  (setq pdf-view-use-scaling t)
  (setq pdf-view-use-imagemagick nil)
  (setq pdf-annot-activate-created-annotations nil))

(use-package pdf-occur
  :hook (pdf-view-mode . pdf-occur-global-minor-mode))

(use-package pdf-history
  :hook (pdf-view-mode . pdf-history-minor-mode))

(use-package pdf-links
  :hook (pdf-view-mode . pdf-links-minor-mode))

(use-package pdf-outline
  :hook (pdf-view-mode . pdf-outline-minor-mode)
  :bind (:map pdf-outline-buffer-mode-map
              ("RET" . pdf-outline-follow-link-and-quit)))

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
  (with-eval-after-load 'pdf-view
    (define-key pdf-view-mode-map [remap pdf-misc-print-document] #'mrb/pdf-misc-print-pages)))

(use-package saveplace-pdf-view
  :load-path "packages/saveplace-pdf-view/"
  :defer t)

(use-package nov
  :load-path ("packages/nov.el/" "packages/esxml/")
  :mode (".epub" . nov-mode)
  :config
  (setq nov-unzip-program (executable-find "bsdtar")
        nov-unzip-args '("-xC" directory "-f" filename)))

(provide 'init-reader)
;;; init-reader.el ends here.
