;; -*- lexical-binding: t; -*-

(with-eval-after-load 'dired
  (setq dired-dwim-target t
        dired-listing-switches "-alh --group-directories-first"
	dired-auto-revert-buffer #'dired-buffer-stale-p
        dired-kill-when-opening-new-dired-buffer t
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
	dired-filename-display-length 'window))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'dired-omit-mode)
(add-hook 'dired-mode-hook #'hl-line-mode)

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package file-info
  :bind ("C-c c i" . file-info-show))

(provide 'init-dired)
