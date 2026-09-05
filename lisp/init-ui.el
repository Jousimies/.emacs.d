;; -*- lexical-binding: t; -*-

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :hook (minibuffer-mode . nerd-icons-completion-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(use-package form-feed
  :hook ((org-mode . form-feed-mode)
	 (emacs-news-mode . form-feed-mode)))


(provide 'init-ui)
