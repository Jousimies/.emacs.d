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
  :diminish gc-buffers-mode
  :hook (after-init . gc-buffers-mode))

(use-package gcmh
  :diminish gcmh-mode
  :hook ((after-init . gcmh-mode)
         (focus-out . garbage-collect))
  :config
  (setq gc-cons-percentage 0.1)
  (setq gcmh-idle-delay 'auto)
  (setq gcmh-auto-idle-delay-factor 10)
  (setq gcmh-high-cons-threshold #x1000000))

(provide 'init-buffer)
;;; init-buffer.el ends here.
