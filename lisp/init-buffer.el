(setq switch-to-buffer-in-dedicated-window 'pop)
(setq switch-to-buffer-obey-display-actions t)

(defun switch-to-message ()
  "Quick switch to `*Message*' buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun switch-to-scratch ()
  "Quick switch to `*Scratch*' buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(my/space-leader-def
  "b" '(:ignore t :wk "Buffer")
  "bs" '(switch-to-scratch :wk "*scratch*")
  "bm" '(switch-to-message :wk "*message*"))

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
