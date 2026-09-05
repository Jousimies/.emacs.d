;; -*- lexical-binding: t; -*-

;; nerd-icons-ibuffer
(add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)

;; nerd-icons-dired
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

;; nerd-icons-completion
(add-hook 'minibuffer-mode-hook #'nerd-icons-completion-mode)

;; rainbow-mode
(add-hook 'prog-mode-hook #'rainbow-mode)

;; goggles
(add-hook 'prog-mode-hook #'goggles-mode)
(add-hook 'text-mode-hook #'goggles-mode)
(with-eval-after-load 'goggles
  (setq-default goggles-pulse t))

;; form-feed
(add-hook 'on-first-buffer-hook #'global-form-feed-mode)

(provide 'init-ui)
