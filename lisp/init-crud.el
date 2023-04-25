;; init-crud.el --- Add, delte, modify and check. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package auto-save
  :demand t
  :config
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t)
  (auto-save-enable))

(use-package undo-fu)

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
  (setq recentf-exclude '(".pdf$"))
  (recentf-mode))

(use-package register
  :bind ("C-c f" . jump-to-register)
  :config
  (set-register ?g (cons 'file (expand-file-name "todos/org-gtd-tasks.org" my-galaxy)))
  (set-register ?b (cons 'file (expand-file-name "denote/books/20230301T211439--Book-lists-and-reading-record__reading.org" my-galaxy)))
  (set-register ?e (cons 'file (concat my-galaxy "/denote/20230330T120149==5d2b3--rss-sources__elfeed_emacs.org")))
  (set-register ?f (cons 'file (expand-file-name "finance/beans/finance.bean" my-galaxy)))
  (set-register ?i (cons 'file (expand-file-name "inbox/inbox.org" my-galaxy)))
  (set-register ?p (cons 'file (expand-file-name "inbox/plan.org" my-galaxy))))

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
