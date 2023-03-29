;; init-dashboard.el --- Dashboard. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-set-init-info t)
  (setq dashboard-set-footer nil)
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-heading-icons nil)
  (setq dashboard-items nil)
  ;; (setq dashboard-items '((recents  . 5)
  ;;                         (bookmarks . 5)
  ;;                         (agenda . 5)))
  (require 'cal-china)
  (let* ((ny (calendar-gregorian-from-absolute
              (cadr (assoc 1 (calendar-chinese-year
                              (string-to-number
                               (format-time-string "%Y" (current-time))))))))
         (m (string-to-number (format-time-string "%m" (current-time))))
         (d (string-to-number (format-time-string "%d" (current-time)))))
    (if (and (= d (cadr ny))
             (= m (car ny)))
        (setq dashboard-startup-banner (expand-file-name "src/banner2.txt" user-emacs-directory))
      (setq dashboard-startup-banner (expand-file-name "src/banner.txt" user-emacs-directory))))
  (setq dashboard-set-navigator nil))

(provide 'init-dashboard)
;;; init-dashboard.el ends here.
