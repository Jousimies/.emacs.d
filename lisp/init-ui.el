;; -*- lexical-binding: t; -*-

(set-face-attribute 'default nil :family "Iosevka" :height 120)
(set-fontset-font t 'unicode (font-spec :family "Symbols Nerd Font Mono" :size 12) nil 'prepend)
(dolist (charset '(kana han cjk-misc bopomofo symbol))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "LXGW WenKai Mono")))

(blink-cursor-mode -1)

(add-hook 'on-first-file-hook #'global-prettify-symbols-mode)
(with-eval-after-load 'prog-mode
  (setopt prettify-symbols-alist '(("lambda" . ?λ)
                                   ("function" . ?𝑓))))

(use-package nerd-icons
  :defer t)

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
