;; init-dashboard.el --- Dashboard. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/packages/emacs-dashboard/")

(require 'dashboard)

(dashboard-setup-startup-hook)

(add-hook 'dashboard-mode-hook (lambda ()
								 (setq-local display-line-numbers nil)))

(with-eval-after-load 'dashboard
  ;; (define-key dashboard-mode-map "q" nil)
  (define-key dashboard-mode-map (kbd "n") #'dashboard-next-line)
  (define-key dashboard-mode-map (kbd "p") #'dashboard-previous-line))

(setq dashboard-icon-type 'nerd-icons)
(setq dashboard-set-file-icons t)
(setq dashboard-center-content t)
(setq dashboard-set-init-info t)
(setq dashboard-set-footer nil)
(setq dashboard-banner-logo-title "EXPLORE THE WORLD, FULFILL YOUR BEING.")

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (registers . 5)))

(provide 'init-dashboard)
;;; init-dashboard.el ends here.
