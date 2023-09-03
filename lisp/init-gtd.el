;; init-gtd.el --- Tasks management. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package org-agenda
  :bind ("C-<f12>" . org-agenda)
  :hook (org-agenda-finalize . #'org-agenda-find-same-or-today-or-agenda)
  :config
  ;; (setq org-agenda-files (directory-files-recursively (expand-file-name "todos" my-galaxy) "org$"))
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-compact-blocks t)
  ;; (setq org-agenda-window-setup 'other-tab)
  (setq org-agenda-align-tags-to-column -120))

(use-package org-gtd
  :diminish org-edna-mode
  :init
  (setq org-gtd-update-ack "3.0.0")
  :custom
  (org-gtd-directory (expand-file-name "iCloud~com~appsonthemove~beorg/Documents/org" mobile-document))
  (org-agenda-property-list '("DELEGATED_TO"))
  (org-gtd-organize-hooks '(org-gtd-set-area-of-focus org-set-tags-command))
  (org-edna-use-inheritance t)
  (org-gtd-areas-of-focus '("Carrer" "Health" "Family" "Finance" "Social" "Spiritual" "Entertainment" "Personal" "Community"))
  :config
  (org-edna-mode)
  ;; (add-to-list 'org-agenda-files (expand-file-name "todos/org-gtd-tasks.org" my-galaxy))
  :bind (("<f12>" . org-gtd-engage)
         ("C-<f12>" . org-gtd-process-inbox)
         ("s-<f12>" . org-gtd-review-stuck-projects)
         (:map org-gtd-clarify-map
               ("C-c c" . org-gtd-organize))))

(use-package calendar
  :bind ("C-c a c" . calendar)
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
  :defer t
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

(provide 'init-gtd)
;;; init-gtd.el ends here.
