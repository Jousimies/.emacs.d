;;; init-reader.el --- Emacs as e-file reader.   -*- lexical-binding: t no-byte-compile: t -*-
;;; Code:
;;; Commentary:

(when (maybe-require-package 'pdf-tools)
  (add-hook 'doc-view-mode-hook (lambda () (require 'pdf-tools)))

  (with-eval-after-load 'pdf-tools
    (pdf-tools-install)
    (setq-default pdf-view-display-size 'fit-width))

  (add-hook 'pdf-tools-enabled-hook
            #'(lambda ()
                (if (string-equal "dark" (frame-parameter nil 'background-mode))
                    (pdf-view-themed-minor-mode 1))))

  (setq pdf-view-use-unicode-ligther nil)
  (setq pdf-view-use-scaling t)
  (setq pdf-view-use-imagemagick nil)
  (setq pdf-annot-activate-created-annotations nil)

  (defun my/get-file-name ()
    "Copy pdf file name."
    (interactive)
    (kill-new (file-name-base (buffer-file-name)))
    (message "Copied %s" (file-name-base (buffer-file-name))))

  (with-eval-after-load 'pdf-view
  ;;   (define-key pdf-view-mode-map (kbd "w") 'my/get-file-name)
  ;;   (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  ;;   (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  ;;   (define-key pdf-view-mode-map (kbd "d") 'pdf-annot-delete)
  ;;   (define-key pdf-view-mode-map (kbd "q") 'kill-this-buffer)
  ;;   (define-key pdf-view-mode-map (kbd "y") 'pdf-view-kill-ring-save)
  ;;   (define-key pdf-view-mode-map (kbd "G") 'pdf-view-goto-page))
    (define-key pdf-view-mode-map [remap pdf-misc-print-document] 'mrb/pdf-misc-print-pages))

  (with-eval-after-load 'pdf-outline
    (define-key pdf-outline-buffer-mode-map (kbd "<RET>") 'pdf-outline-follow-link-and-quit))

  (with-eval-after-load 'pdf-annot
    (define-key pdf-annot-edit-contents-minor-mode-map (kbd "<return>") 'pdf-annot-edit-contents-commit)
    (define-key pdf-annot-edit-contents-minor-mode-map (kbd "<S-return>") 'newline))

  (with-eval-after-load 'pdf-cache
    (define-pdf-cache-function pagelabels))

  (with-eval-after-load 'pdf-misc
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
        (pdf-misc-print-document filename)))))

(when (maybe-require-package 'nov)
  (setq nov-unzip-program (executable-find "bsdtar")
        nov-unzip-args '("-xC" directory "-f" filename))
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(provide 'init-reader)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-reader.el ends here
