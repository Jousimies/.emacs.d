;; -*- lexical-binding: t; -*-
;; (setq initial-buffer-choice 'dashboard-open)
(dashboard-setup-startup-hook)

(add-hook 'dashboard-mode-hook (lambda ()
				 (setq-local display-line-numbers nil)))

(with-eval-after-load 'dashboard
  (setq dashboard-startup-banner (expand-file-name "src/bitmap.png" user-emacs-directory))
  (setq dashboard-image-banner-max-width 500)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-set-init-info t)
  (setq dashboard-week-agenda nil)
  (setq dashboard-set-footer nil)
  (setq dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     (registers . 5)))
  (setq dashboard-banner-logo-title "EXPLORE THE WORLD, FULFILL YOUR BEING."))


(provide 'init-dashboard)
