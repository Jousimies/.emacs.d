;; init-builtin.el --- Emacs Builtin. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; Variables defined in C source code
(setq ring-bell-function 'ignore)
(setq tab-width 4)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq use-short-answers t)
(setq read-process-output-max #x10000)
(setq create-lockfiles nil)
(setq recenter-redisplay nil)
(setq load-prefer-newer t)
(setq next-screen-context-lines 5)
(setq frame-inhibit-implied-resize t)
(setq inhibit-compacting-font-caches t)
(setq inhibit-quit nil)
(setq fast-but-imprecise-scrolling t)
(setq scroll-preserve-screen-position 'always)
(setq auto-save-list-file-name nil)
(setq history-delete-duplicates t)
(setq bidi-display-reordering nil)
(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)
(setq delete-by-moving-to-trash t)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(setq max-mini-window-height 10)

;; Define some variables to facilitate the location of configuration files or related settings for specific systems.
(defvar mobile-document "~/Library/Mobile Documents/"
  "This folder contains documents in icloud.")

(defvar my-cloud "~/Nextcloud"
  "This folder is My cloud.")

;; L.Personal.Galaxy location may change, but folders in this directory never change.
(defvar my-galaxy (expand-file-name "L.Personal.Galaxy" my-cloud)
  "This folder stores all the plain text files of my life.")

(defvar website-directory (expand-file-name "blogs_source/" my-galaxy)
  "The source folder of my blog.")

(defvar my/web_archive (expand-file-name "web_archive/" my-galaxy)
  "The folder save web pages.")

(defvar my/reference-lists `(,(concat my-galaxy "/bibtexs/References.bib")
                             ,(concat my-galaxy "/bibtexs/Books.bib")))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(use-package server
  :hook (after-init . (lambda ()
                        (unless (server-running-p)
                          (server-start)))))

(setq auto-save-list-file-prefix (expand-file-name "cache/auto-save-list/.saves-" user-emacs-directory))
;; (setq inhibit-default-init t)
(setq inhibit-startup-screen t)
;; (setq inhibit-splash-screen t)

(use-package simple
  :diminish visual-line-mode
  :bind ("C-c b s" . scratch-buffer)
  :hook ((prog-mode . column-number-mode)
         (text-mode . size-indication-mode)
         (text-mode . turn-on-visual-line-mode)
         (org-mode . turn-on-visual-line-mode)
         (LaTeX-mode . turn-on-visual-line-mode))
  :init
  (setq-default indent-tabs-mode nil)
  :config
  ;; (setq read-extended-command-predicate #'command-completion-default-include-point)
  (setq mark-ring-max 128)
  (setq kill-do-not-save-duplicates t)
  (setq kill-ring-max (* kill-ring-max 2))
  (setq async-shell-command-display-buffer nil))

(use-package files
  :defer t
  :config
  (setq trash-directory "~/.Trash")
  (setq auto-save-default nil)
  (setq large-file-warning-threshold nil)
  (setq confirm-kill-processes nil)
  (setq confirm-kill-emacs nil)
  ;; (setq make-backup-files t)
  ;; (setq backup-directory-alist '(("." . "~/.emacs.d/cache/backups")))
  (setq view-read-only t)
  (setq kill-read-only-ok t)
  ;; https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/
  (defun my/auto-create-missing-dirs ()
    (let ((target-dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p target-dir)
        (make-directory target-dir t))))
  (add-to-list 'find-file-not-found-functions #'my/auto-create-missing-dirs))

(use-package ffap
  :bind ("C-c f f" . find-file-at-point))

(use-package message
  :bind ("C-c b m" . switch-to-message)
  :config
  (defun switch-to-message ()
    "Quick switch to `*Message*' buffer."
    (interactive)
    (switch-to-buffer "*Messages*"))
  (setq message-kill-buffer-on-exit t)
  (setq message-kill-buffer-query nil)
  (setq message-sendmail-envelope-from 'header)
  (setq message-kill-buffer-query nil)
  (setq message-sendmail-extra-arguments '("-a" "outlook")))

(use-package calc
  :bind ("C-c a C" . calc)
  :hook ((calc-trail-mode . (lambda ()
                              (setq-local mode-line-format nil)))
         (calc-mode . (lambda ()
                        (setq-local mode-line-format nil))))
  :config
  (setq calc-window-height 15))

(use-package so-long
  :hook (after-init . global-so-long-mode))

(use-package prog-mode
  :hook ((prog-mode . prettify-symbols-mode)
         (LaTeX-mode . prettify-symbols-mode))
  :config
  (setq prettify-symbols-alist '(("lambda" . ?λ)
                                 ("function" . ?𝑓))))

(use-package outline
  :diminish outline-minor-mode
  :hook (prog-mode . outline-minor-mode))

(use-package pixel-scroll
  :hook (after-init . pixel-scroll-precision-mode))

(use-package doc-view
  :defer t
  :config
  (setq doc-view-mupdf-use-svg t)
  (setq doc-view-imenu-flatten t)
  (setq doc-view-continuous t))

(use-package abbrev
  :diminish abbrev-mode
  :hook ((org-mode . abbrev-mode)
         (LaTeX-mode . abbrev-mode)))

(use-package bookmark
  :defer t
  :config
  (setq bookmark-default-file (expand-file-name "cache/bookmarks" user-emacs-directory)))

(use-package url
  :defer t
  :config
  (setq url-configuration-directory (expand-file-name "cache/url" user-emacs-directory)))

(use-package multisession
  :defer t
  :config
  (setq multisession-directory (expand-file-name "cache/multisession" user-emacs-directory)))

(use-package cursor-sensor
  :hook (minibuffer-setup . cursor-intangible-mode))

(use-package midnight
  :hook (after-init . midnight-mode))

(use-package word-wrap-mode
  :diminish word-wrap-whitespace-mode
  :hook (org-mode . word-wrap-whitespace-mode))

(use-package transient
  :defer t
  :config
  (setq transient-levels-file (expand-file-name "cache/transient/levels.el" user-emacs-directory))
  (setq transient-values-file (expand-file-name "cache/transient/values.el" user-emacs-directory))
  (setq transient-history-file (expand-file-name "cache/transient/history.el" user-emacs-directory)))

(provide 'init-builtin)
;;; init-builtin.el ends here.
