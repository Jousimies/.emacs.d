;;; init-recentf.el --- Recentf. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(add-hook 'after-init-hook 'recentf-mode)
;; (run-with-idle-timer 1 nil (lambda ()
;;                              (recentf-cleanup)))
(with-eval-after-load 'recentf
  (setq recentf-max-saved-items 1000)
  (setq recentf-exclude nil))

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
  (add-hook 'org-mode-hook 'hungry-delete-mode))

;; Auto save
(add-to-list 'load-path (expand-file-name "site-lisp/auto-save" user-emacs-directory))
(require 'auto-save)
(auto-save-enable)
(setq auto-save-silent t)
(setq auto-save-delete-trailing-whitespace t)


(provide 'init-recentf)
;;; init-recentf.el ends here
