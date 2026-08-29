;; -*- lexical-binding: t; -*-


(when (and sys/macp (executable-find "gls"))
  (setopt dired-use-ls-dired nil)
  (setopt insert-directory-program "gls")
  (setopt dired-listing-switches
	  "-l --almost-all --human-readable --group-directories-first --no-group"))

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
