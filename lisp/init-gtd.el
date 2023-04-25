;; init-gtd.el --- Tasks management. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package org-agenda
  :bind ("C-<f12>" . org-agenda)
  :hook (org-agenda-finalize . #'org-agenda-find-same-or-today-or-agenda)
  :config
  (setq org-agenda-files (directory-files-recursively (expand-file-name "todos" my-galaxy) "org$"))
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-window-setup 'other-tab)
  (setq org-agenda-align-tags-to-column -120))

;;;###autoload
(defun my/gtd-file ()
  (interactive)
  (find-file (expand-file-name "todos/org-gtd-tasks.org" my-galaxy)))

(use-package org-gtd
  :commands org-gtd-capture org-gtd-engage org-gtd-process-inbox org-gtd-show-all-next org-gtd-show-stuck-projects
  :bind (("<f12>" . org-gtd-engage)
         ("C-<f12>" . org-gtd-process-inbox)
         ("s-<f12>" . org-gtd-show-stuck-projects)
         (:map org-gtd-process-map
               ("C-c c" . org-gtd-choose)))
  :init
  (setq org-gtd-update-ack "2.1.0")
  :custom
  (org-gtd-directory (expand-file-name "todos" my-galaxy))
  (org-agenda-property-list '("DELEGATED_TO"))
  :config
  (add-to-list 'org-agenda-files (expand-file-name "todos/org-gtd-tasks.org" my-galaxy)))

(use-package org-edna
  :after org-gtd
  :config
  (setq org-edna-use-inheritance t)
  (org-edna-load))

(use-package calendar
  :defer t
  :config
  (setq calendar-view-diary-initially-flag t)
  (setq calendar-mark-diary-entries-flag t)
  (setq calendar-mode-line-format nil)

  (setq calendar-date-style 'iso)
  (setq calendar-date-display-form calendar-iso-date-display-form)
  (add-hook 'calendar-today-visible-hook #'calendar-mark-today)

  (setq calendar-time-display-form
        '(24-hours ":" minutes
                   (when time-zone
                     (format "(%s)" time-zone))))
  (setq diary-date-forms diary-iso-date-forms))

(use-package appt
  :after calendar
  :hook (diary-mode . appt-activate)
  :config
  (setq appt-display-diary nil)
  (setq appt-disp-window-function #'appt-disp-window)
  (setq appt-display-mode-line t)
  (setq appt-display-interval 3)
  (setq appt-audible nil)
  (setq appt-warning-time-regexp "appt \\([0-9]+\\)")
  (setq appt-message-warning-time 6))

(use-package diary-lib
  :after calendar
  :config
  (add-hook 'diary-list-entries-hook #'diary-sort-entries)
  (add-hook 'diary-mode-hook #'goto-address-mode)
  (setq diary-display-function #'diary-fancy-display)
  (setq diary-header-line-format nil)
  (setq diary-list-include-blanks nil)
  (setq diary-abbreviated-year-flag nil)
  (setq diary-number-of-entries 7)
  (setq diary-comment-start ");;")
  (setq diary-comment-end "")
  (setq diary-nonmarking-symbol "!")

  (setq diary-file (expand-file-name "diary/diary.org" my-galaxy)))

(use-package alert
  :commands alert
  :config
  (setq alert-default-style 'osx-notifier))

(use-package alarm-clock
  :bind ("C-M-<f12>" . alarm-clock-set)
  :commands (alarm-clock-set alarm-clock-list-view)
  :config
  (setq alarm-clock-play-sound nil)
  (setq alarm-clock-cache-file (expand-file-name "var/.alarm-clock.cache" user-emacs-directory)))

(use-package pomm
  :bind ("M-<f12>" . pomm)
  :config
  (setq pomm-state-file-location (expand-file-name "cache/pomm" user-emacs-directory))
  (pomm-mode-line-mode))

(provide 'init-gtd)
;;; init-gtd.el ends here.
