;; init-input-method.el --- Input methods. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;; Earlier, I use emacs-rime. Now, sis I selected.

;;; Code:

(use-package sis
  :config
  (face-spec-set 'sis-inline-face
                 '((((background light))
                    :foreground "black" :background "#94d4ff")
                   (t
                    :foreground "black" :background "#a4d5f9"))
                 'face-override-spec)

  (sis-ism-lazyman-config "com.apple.keylayout.ABC" "im.rime.inputmethod.Squirrel.Hans")
  (setq sis-external-ism "im-select")
  (setq sis-other-cursor-color "red")

  (sis-global-cursor-color-mode t)
  (sis-global-context-mode t)
  (sis-global-respect-mode t)
  (sis-global-inline-mode t)

  (add-to-list 'sis-context-detectors
               (lambda (&rest _)
                 (when (and (eq major-mode 'org-mode)
                            (and (not (org-at-clock-log-p))
                                 (not (org-at-table-p))
                                 (not (org-in-src-block-p))
                                 (not (org-at-timestamp-p))))
                   'other))))

(use-package rime
  :defer t
  :init
  (setq rime-user-data-dir "~/Library/Rime/")
  (setq rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include/")
  (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory)))

(use-package rime-regexp
  :config
  (rime-regexp-mode))

(provide 'init-input-method)
;;; init-input-method.el ends here.
