(setq auto-save-list-file-prefix (expand-file-name "cache/auto-save-list/.saves-" user-emacs-directory))
(setq inhibit-default-init t)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

(use-package simple
  :hook ((prog-mode . column-number-mode)
         (text-mode . size-indication-mode)
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
  (setq auto-save-default nil)
  (setq large-file-warning-threshold nil)
  (setq confirm-kill-processes nil)
  (setq confirm-kill-emacs nil)
  (setq make-backup-files nil)
  (setq view-read-only t)
  (setq kill-read-only-ok t)

  (defun my/auto-create-missing-dirs ()
    (let ((target-dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p target-dir)
        (make-directory target-dir t))))
  (add-to-list 'find-file-not-found-functions #'my/auto-create-missing-dirs))

(use-package ffap
  :defer t
  :config
  (setq ffap-machine-p-known 'reject))

(use-package calc
  :general (my/space-leader-def
             "C" '(calc :wk "calc"))
  :hook ((calc-trail-mode . (lambda ()
                              (setq-local mode-line-format nil)))
         (calc-mode . (lambda ()
                        (setq-local mode-line-format nil))))
  :config
  (setq calc-window-height 15))

(use-package autorevert
  :hook (text-mode . global-auto-revert-mode))

(add-hook 'prog-mode-hook 'outline-minor-mode)

(use-package doc-view
  :defer t
  :config
  (setq doc-view-mupdf-use-svg t)
  (setq doc-view-imenu-flatten t)
  (setq doc-view-continuous t))

(use-package bookmark
  :general (my/space-leader-def
             "ba" 'bookmark-set
             "br" 'bookmark-rename
             "bd" 'bookmark-delete
             "bj" 'bookmark-jump)
  :config
  (setq bookmark-default-file (expand-file-name "cache/bookmarks" user-emacs-directory)))

(use-package select
  :defer t
  :config
  (setq select-enable-primary t))

(use-package url
  :defer t
  :config
  (setq url-configuration-directory (expand-file-name "cache/url" user-emacs-directory)))

(use-package multisession
  :defer t
  :config
  (setq multisession-directory (expand-file-name "cache/multisession" user-emacs-directory)))

(provide 'init-builtin)
;;; init-builtin.el ends here.
