(use-package recentf
  :hook (window-setup . recentf-mode)
  :config
  (setq recentf-save-file (expand-file-name "cache/recentf" user-emacs-directory))
  (setq recentf-auto-cleanup 300)
  (setq recentf-max-saved-items 1000))

(provide 'init-recentf)
;;; init-recentf.el ends here.
