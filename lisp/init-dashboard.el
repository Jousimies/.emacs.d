;;; init-dashboard.el --- 	-*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(when (require-package 'dashboard)

  (setq dashboard-startup-banner (expand-file-name "banner.txt" user-emacs-directory))
  (setq dashboard-center-content t)
  (setq dashboard-set-init-info t)
  ;; (setq dashboard-set-file-icons t)
  ;; (setq dashboard-items '((recents  . 5)
  ;;                         (bookmarks . 5)
  ;;                         (registers . 5)))
  (setq dashboard-items nil)
  ;; (setq dashboard-set-navigator t)
  (add-hook 'after-init-hook 'dashboard-setup-startup-hook))

(provide 'init-dashboard)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dashboard.el ends here
