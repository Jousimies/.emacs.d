(setq switch-to-buffer-in-dedicated-window 'pop)
(setq switch-to-buffer-obey-display-actions t)

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
