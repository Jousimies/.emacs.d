;; init-buffer.el --- Buffer manipulate. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-default-sorting-mode 'major-mode))

(use-package ibuf-ext
  :hook (ibuffer-mode . ibuffer-auto-mode))

(use-package gc-buffers
  :config
  (gc-buffers-mode))

(use-package gcmh
  :hook ((after-init . gcmh-mode)
         (focus-out . garbage-collect))
  :config
  (setq gc-cons-percentage 0.1)
  (setq gcmh-idle-delay 'auto)
  (setq gcmh-auto-idle-delay-factor 10)
  (setq gcmh-high-cons-threshold #x1000000))

(with-eval-after-load 'outline
  (define-key outline-minor-mode-map (kbd "C-<tab>") 'bicycle-cycle)
  (define-key outline-minor-mode-map (kbd "S-<tab>") 'bicycle-cycle-global))

(use-package midnight
  :config
  (midnight-mode))

(use-package word-wrap-mode
  :hook (org-mode . word-wrap-whitespace-mode))

(provide 'init-buffer)
;;; init-buffer.el ends here.
