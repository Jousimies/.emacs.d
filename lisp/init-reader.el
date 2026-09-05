;; -*- lexical-binding: t; -*-

(use-package nov
  :mode (".epub" . nov-mode)
  :custom
  (nov-unzip-program (executable-find "bsdtar"))
  (nov-unzip-args '("-xC" directory "-f" filename))
  (nov-save-place-file (expand-file-name "nov_place" cache-directory)))

;; 在 win 上使用需要在 Mingw 中安装  mingw-w64-x86_64-emacs-pdf-tools-server。
(use-package pdf-tools
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

(use-package pdf-roll
  :hook (pdf-view-mode . pdf-view-roll-minor-mode))

(use-package pdf-occur
  :hook (pdf-view-mode . pdf-occur-global-minor-mode))

(use-package pdf-history
  :hook (pdf-view-mode . pdf-history-minor-mode))

(use-package pdf-links
  :hook (pdf-view-mode . pdf-links-minor-mode))

(use-package pdf-outline
  :hook ((pdf-view-mode . pdf-outline-minor-mode))
  :bind (:map pdf-outline-buffer-mode-map
              ("RET" . pdf-outline-follow-link-and-quit)))

;; 将 pdf-outline-buffer-mode 默认视为 Emacs 状态
(eval-after-load 'viper
  '(add-to-list 'viper-emacs-state-mode-list 'pdf-outline-buffer-mode))

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
  (when sys/macp
    (setq pdf-misc-print-program-executable "/usr/bin/lp"))
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
      (pdf-misc-print-document filename))))

(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map [remap pdf-misc-print-document] #'mrb/pdf-misc-print-pages))

(use-package saveplace-pdf-view
  :after pdf-tools)

(provide 'init-reader)
