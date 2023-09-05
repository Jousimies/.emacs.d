;; init-crud.el --- Add, delte, modify and check. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package auto-save
  :hook (after-init . auto-save-enable)
  :config
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t))

(use-package undo-fu-session
  :hook (after-init . undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-directory (expand-file-name "cache/undo-fu-session" user-emacs-directory))

  (defun my/undo-fu-session--make-file-name (filename)
    "Take the path FILENAME and return a name base on this."
    (concat
     (file-name-concat undo-fu-session-directory
                       (md5 (convert-standard-filename (expand-file-name filename))))
     (undo-fu-session--file-name-ext)))
  (advice-add 'undo-fu-session--make-file-name :override #'my/undo-fu-session--make-file-name))

(use-package vundo
  :bind ("s-z" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (expand-file-name "cache/history" user-emacs-directory))
  (setq history-length 1000
        savehist-additional-variables '(kill-ring
                                        search-ring
                                        regexp-search-ring)
        history-delete-duplicates t))

(use-package saveplace
  :hook (after-init . save-place-mode)
  :config
  (setq save-place-file (expand-file-name "cache/places" user-emacs-directory)))

(use-package recentf
  :hook (after-init . recentf-mode)
  :bind ("C-c f r" . recentf-open-files)
  :config
  (setq recentf-save-file (expand-file-name "cache/recentf" user-emacs-directory))
  (setq recentf-auto-cleanup 300)
  (setq recentf-max-saved-items 1000)
  (setq recentf-exclude '(".pdf$")))

(use-package register
  :bind ("C-x r j" . jump-to-register)
  :custom
  (register-preview-delay 0)
  :config
  (set-register ?g (cons 'file (expand-file-name "todos/org-gtd-tasks.org" my-galaxy)))
  (set-register ?b (cons 'file (expand-file-name "denote/books/20230301T211439--Book-lists-and-reading-record__reading.org" my-galaxy)))
  (set-register ?e (cons 'file (concat my-galaxy "/denote/20230330T120149==5d2b3--rss-sources__elfeed_emacs.org")))
  (set-register ?f (cons 'file (expand-file-name "finance/beans/finance.bean" my-galaxy)))
  (set-register ?i (cons 'file (expand-file-name "inbox/inbox.org" my-galaxy)))
  (set-register ?p (cons 'file (expand-file-name "inbox/plan.org" my-galaxy))))

(use-package autorevert
  :hook (text-mode . global-auto-revert-mode))

(use-package hungry-delete
  :diminish hungry-delete-mode
  :custom
  (hungry-delete-chars-to-skip " \t\n\r\f\v")
  :hook ((text-mode . hungry-delete-mode)
         (prog-mode . hungry-delete-mode)
         (org-mode . hungry-delete-mode)))

(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :hook (after-init . global-whitespace-cleanup-mode))

(use-package ace-pinyin
  :hook (after-init . ace-pinyin-global-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

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

(use-package tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :config
  (setq tempel-path `("~/.emacs.d/template/tempel"
                      ,(expand-file-name "config/tempel" my-galaxy))))

(use-package rime
  :hook ((meow-insert-enter . (lambda ()
                                (rime-activate nil)))
         (meow-insert-enter . (lambda ()
                                (if (and (not (rime--should-inline-ascii-p))
                                         (eq major-mode 'org-mode)
                                         (not (org-at-clock-log-p))
                                         (not (org-at-table-p))
                                         (not (org-at-timestamp-p))
                                         (not (and (bolp) (org-on-heading-p))))
                                    (progn
                                      (activate-input-method "rime")
                                      (im-change-cursor-color)))))
         (meow-insert-exit . (lambda ()
                               (deactivate-input-method)
                               (set-cursor-color (foreground-color-at-point)))))
  :config
  (setq rime-title " ")
  (defvar im-cursor-color "red"
    "The color for input method.")

  (defun im--chinese-p ()
    "Check if the current input state is Chinese."
    (if (featurep 'rime)
        (and (rime--should-enable-p)
             (not (rime--should-inline-ascii-p))
             current-input-method)
      current-input-method))

  (defun im-change-cursor-color ()
    "Set cursor color depending on input method."
    (interactive)
    (set-cursor-color (if (im--chinese-p)
                          im-cursor-color
                        (foreground-color-at-point))))

  (setq default-input-method "rime")
  (setq rime-user-data-dir "~/Library/Rime/")
  (setq rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include/")
  (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
  (setq rime-disable-predicates '(rime-predicate-prog-in-code-p
                                  rime-predicate-org-in-src-block-p
                                  rime-predicate-org-latex-mode-p
                                  rime-predicate-tex-math-or-command-p))
  (setq rime-inline-predicates '(rime-predicate-space-after-cc-p
                                 rime-predicate-after-alphabet-char-p)))

(use-package rime-regexp
  :hook (minibuffer-mode . rime-regexp-mode))

(provide 'init-crud)
;;; init-crud.el ends here.
