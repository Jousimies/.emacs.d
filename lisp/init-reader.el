;; -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(with-eval-after-load 'nov
  (setq nov-unzip-program (executable-find "bsdtar"))
  (setq nov-unzip-args '("-xC" directory "-f" filename))
  (setq nov-save-place-file (expand-file-name "nov_place" cache-directory)))



;; 在 win 上使用需要在 Mingw 中安装  mingw-w64-x86_64-emacs-pdf-tools-server。
(let ((epdfinfo (expand-file-name "packages/pdf-tools/server/epdfinfo" user-emacs-directory)))
  (when (file-executable-p epdfinfo)
    (setq pdf-info-epdfinfo-program epdfinfo)))

(add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdf-view-mode))
(add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))

(with-eval-after-load 'pdf-tools
  (pdf-tools-install t nil t nil))

(with-eval-after-load 'pdf-view
  (add-hook 'pdf-tools-enabled-hook #'pdf-view-themed-minor-mode)
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (require 'saveplace-pdf-view)))

  (setq pdf-view-display-size 'fit-width)
  (setq pdf-view-use-unicode-ligther nil)
  (setq pdf-view-use-scaling t)
  (setq pdf-view-use-imagemagick nil)
  (setq pdf-annot-activate-created-annotations nil))

(with-eval-after-load 'pdf-roll
  (add-hook 'pdf-view-mode-hook #'pdf-view-roll-minor-mode))

;; pdf-occur
(with-eval-after-load 'pdf-view
  (add-hook 'pdf-view-mode-hook
            (lambda () (pdf-occur-global-minor-mode 1))))

;; pdf-history
(with-eval-after-load 'pdf-history
  (add-hook 'pdf-view-mode-hook #'pdf-history-minor-mode))

;; pdf-links
(with-eval-after-load 'pdf-links
  (add-hook 'pdf-view-mode-hook #'pdf-links-minor-mode))

;; pdf-outline
(with-eval-after-load 'pdf-outline
  (add-hook 'pdf-view-mode-hook #'pdf-outline-minor-mode)
  (define-key pdf-outline-buffer-mode-map (kbd "RET") #'pdf-outline-follow-link-and-quit))

;; pdf-outline-buffer-mode in viper emacs state
(with-eval-after-load 'viper
  (add-to-list 'viper-emacs-state-mode-list 'pdf-outline-buffer-mode))

;; pdf-annot
(with-eval-after-load 'pdf-annot
  (add-hook 'pdf-view-mode-hook #'pdf-annot-minor-mode)
  (define-key pdf-annot-edit-contents-minor-mode-map (kbd "<return>") #'pdf-annot-edit-contents-commit)
  (define-key pdf-annot-edit-contents-minor-mode-map (kbd "<S-return>") #'newline))

;; pdf-sync
(with-eval-after-load 'pdf-sync
  (add-hook 'pdf-view-mode-hook #'pdf-sync-minor-mode))

;; pdf-cache
(with-eval-after-load 'pdf-cache
  (define-pdf-cache-function pagelabels))

;; pdf-misc
(with-eval-after-load 'pdf-misc
  (when sys/macp
    (setq pdf-misc-print-program-executable "/usr/bin/lp"))

  (defun mrb/pdf-misc-print-pages (filename pages &optional interactive-p)
    "Wrapper for `pdf-misc-print-document` to add page selection support."
    (interactive (list (pdf-view-buffer-file-name)
                       (read-string "Page range (empty for all pages): "
                                    (number-to-string (pdf-view-current-page)))
                       t) pdf-view-mode)
    (let ((pdf-misc-print-program-args
           (if (not (string-blank-p pages))
               (cons (concat "-P " pages) pdf-misc-print-program-args)
             pdf-misc-print-program-args)))
      (pdf-misc-print-document filename))))

;; Remap print command in pdf-view-mode
(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map [remap pdf-misc-print-document] #'mrb/pdf-misc-print-pages))

(with-eval-after-load 'pdf-tools
  (require 'saveplace-pdf-view))


(provide 'init-reader)
