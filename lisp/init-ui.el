;; -*- lexical-binding: t; -*-

(use-package nerd-icons-ibuffer
  :idle t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-dired
  :idle t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :idle t
  :hook (minibuffer-mode . nerd-icons-completion-mode))

(use-package rainbow-mode
  :idle t
  :hook (prog-mode . rainbow-mode))

(use-package goggles
  :idle t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(use-package form-feed
  :idle t
  :hook ((org-mode . form-feed-mode)
	 (emacs-news-mode . form-feed-mode)))


(provide 'init-ui)
