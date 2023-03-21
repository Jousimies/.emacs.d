(setq switch-to-buffer-in-dedicated-window 'pop)
(setq switch-to-buffer-obey-display-actions t)

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    "gB" 'switch-to-prev-buffer
    "gb" 'switch-to-buffer
    "zx" 'kill-current-buffer))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package midnight
  :config
  (midnight-mode))

(use-package gc-buffers
  :config
  (gc-buffers-mode))

(provide 'init-buffer)
;;; init-buffer.el ends here.
