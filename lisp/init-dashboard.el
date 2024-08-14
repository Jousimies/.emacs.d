;; init-dashboard.el --- Dashboard. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package dashboard
  :demand t
  :hook (dashboard-mode . (lambda ()
							(setq-local display-line-numbers nil)))
  :bind (:map dashboard-mode-map
			  ("n" . dashboard-next-line)
			  ("p" . dashboard-previous-line))
  :custom
  (dashboard-startup-banner 1)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-set-init-info t)
  (dashboard-set-footer nil)
  (dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     (registers . 5)))
  (dashboard-banner-logo-title "EXPLORE THE WORLD, FULFILL YOUR BEING.")
  :config
  (dashboard-setup-startup-hook))


(provide 'init-dashboard)
;;; init-dashboard.el ends here.
