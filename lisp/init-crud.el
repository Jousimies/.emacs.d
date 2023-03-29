;; init-crud.el --- Add, delte, modify and check. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package files
  :config
  (auto-save-visited-mode 1))

(use-package undo-fu-session
  :after undo-fu
  :config
  (setq undo-fu-session-directory (expand-file-name "cache/undo-fu-session" user-emacs-directory))

  (defun my/undo-fu-session--make-file-name (filename)
    "Take the path FILENAME and return a name base on this."
    (concat
     (file-name-concat undo-fu-session-directory
                       (md5 (convert-standard-filename (expand-file-name filename))))
     (undo-fu-session--file-name-ext)))
  (advice-add 'undo-fu-session--make-file-name :override #'my/undo-fu-session--make-file-name)
  (undo-fu-session-global-mode))

(use-package vundo
  :bind ("C-x u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package savehist
  :config
  (setq savehist-file (expand-file-name "cache/history" user-emacs-directory))
  (setq history-length 1000
        savehist-save-minibuffer-history 1
        savehist-additional-variables '(kill-ring
                                        search-ring
                                        regexp-search-ring)
        history-delete-duplicates t)
  (savehist-mode))

(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "cache/places" user-emacs-directory))
  (save-place-mode))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "cache/recentf" user-emacs-directory))
  (setq recentf-auto-cleanup 300)
  (setq recentf-max-saved-items 1000)
  (recentf-mode))

(use-package autorevert
  :hook (text-mode . global-auto-revert-mode))

(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")

(use-package hungry-delete
  :custom
  (hungry-delete-chars-to-skip " \t\n\r\f\v")
  :hook ((text-mode . hungry-delete-mode)
         (prog-mode . hungry-delete-mode)
         (org-mode . hungry-delete-mode)))

(use-package whitespace-cleanup-mode
  :config
  (setq whitespace-cleanup-mode-preserve-point t)
  (global-whitespace-cleanup-mode 1))

(use-package expand-region
  :commands er/expand-region)

(evil-define-key 'visual 'evil-visual-state-map
    "v" 'er/expand-region)

(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)))

(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'org-mode-hook 'electric-pair-mode)

(use-package delsel
  :hook (text-mode . delete-selection-mode))

(use-package select
  :defer t
  :config
  (setq select-enable-primary t))

(provide 'init-crud)
;;; init-crud.el ends here.
