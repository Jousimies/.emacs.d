;; init-dashboard.el --- Dashboard. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package dashboard
  :load-path "packages/emacs-dashboard/"
  :demand t
  ;; :hook (dashboard-mode . (lambda ()
  ;;                           (setq-local mode-line-format nil)))
  :bind ("C-c b d" . dashboard-open)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-file-icons t)
  ;; (setq dashboard-set-heading-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-set-init-info t)
  (setq dashboard-set-footer nil)
  (setq dashboard-banner-logo-title "EXPLORE THE WORLD, FULFILL YOUR BEING.")
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (registers . 5)))
  (require 'cal-china)
  ;; (defvar dashboard-banner '("xemacs_color.svg" "gnu_color.svg" "banner.txt"))
  (setq dashboard-startup-banner (expand-file-name "src/banner.txt" user-emacs-directory))
  ;; (let* ((ny (calendar-gregorian-from-absolute
  ;;             (cadr (assoc 1 (calendar-chinese-year
  ;;                             (string-to-number
  ;;                              (format-time-string "%Y" (current-time))))))))
  ;;        (m (string-to-number (format-time-string "%m" (current-time))))
  ;;        (d (string-to-number (format-time-string "%d" (current-time))))
  ;;        (file (nth (random (length dashboard-banner)) dashboard-banner)))
  ;;   (if (and (= d (cadr ny))
  ;;            (= m (car ny)))
  ;;       (setq dashboard-startup-banner (expand-file-name "src/banner2.txt" user-emacs-directory))
  ;;     (setq dashboard-startup-banner (expand-file-name (concat "src/" file) user-emacs-directory))
  ;;     ))
  )
;; (setq dashboard-set-navigator nil)
;; (run-with-idle-timer (* 5 60) t 'dashboard-open))
(defun my/update-dashboard ()
  (interactive)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
						  (agenda . 5)
                          (registers . 5)))
  (revert-buffer))
;; (add-hook 'dashboard-after-initialize-hook #'my/update-dashboard)
(run-with-timer 1 nil #'my/update-dashboard)

(provide 'init-dashboard)
;;; init-dashboard.el ends here.
