;;; init-recentf.el --- Recentf. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(with-eval-after-load 'recentf
  (add-hook 'after-init-hook 'recentf-mode)

  (add-hook 'before-save-hook (lambda ()
                                (recentf-cleanup)))

  (setq recentf-max-saved-items 1000)

  (with-eval-after-load 'org-roam
    (add-to-list 'recentf-exclude org-roam-directory))

  (add-to-list 'recentf-exclude "\\.pdf\\'"))

;;Undo-fu, undo-fu-session and vundo
(require-package 'undo-fu)
(require-package 'undo-fu-session)
(require-package 'vundo)

(add-hook 'after-init-hook 'global-undo-fu-session-mode)

(with-eval-after-load 'vundo
  (setq vundo-glyph-alist vundo-unicode-symbols))

(global-set-key (kbd "C-x u") 'vundo)

(when (maybe-require-package 'hungry-delete)
  (setq-default hungry-delete-chars-to-skip " \t\n\r\f\v")
  (add-hook 'text-mode-hook 'hungry-delete-mode)
  (add-hook 'prog-mode-hook 'hungry-delete-mode)
  (add-hook 'org-mode-hook 'hungry-delete-mode))

;; Auto save
;; (require 'auto-save)
(auto-save-enable)
(setq auto-save-silent t)
(setq auto-save-delete-trailing-whitespace t)


(provide 'init-recentf)
;;; init-recentf.el ends here
