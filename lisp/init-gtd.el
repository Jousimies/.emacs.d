;; init-gtd.el --- Tasks management. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package org-agenda
  :bind ("C-<f12>" . org-agenda)
  :hook (org-agenda-finalize . #'org-agenda-find-same-or-today-or-agenda)
  :config
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-todo-ignore-scheduled 'future)
  (setq org-agenda-todo-ignore-deadlines 'near)
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-align-tags-to-column 'auto)
  (setq org-agenda-custom-commands
        '(("r" "Reading Lists"
           ((tags-todo "+Reading|TODO"
                       (
                        (org-agenda-prefix-format " %i")
                        (org-agenda-overriding-header "Reading Lists"))))))))

(use-package org-gtd
    :load-path ("packages/org-gtd.el/" "packages/org-agenda-property" "packages/org-edna")
  :init
  (setq org-gtd-update-ack "3.0.0")
  :custom
  (org-gtd-directory (expand-file-name "iCloud~com~appsonthemove~beorg/Documents/org" mobile-document))
  (org-agenda-property-list '("DELEGATED_TO"))
  (org-gtd-organize-hooks '(org-gtd-set-area-of-focus org-set-tags-command))
  (org-edna-use-inheritance t)
  (org-gtd-areas-of-focus '("Family Life"
                            "Career"
                            "Health and wellness"
                            "Community involvement"
                            "Financial management"
                            "Travel and exploration"
                            "Hobbies and Interests"
                            "Personal development"
                            "Social and relationships"
                            "Retirement planning"))
  (org-gtd-engage-prefix-width 24)
  (org-gtd-clarify-show-horizons 'right)
  :config
  (setq org-agenda-files `(,(expand-file-name "org-gtd-tasks.org" org-gtd-directory)
                           ,(expand-file-name "org-gtd-tasks.org_archive" org-gtd-directory)))

  (org-edna-mode)
  :bind (("<f12>" . org-gtd-engage)
         ("s-<f12>" . org-gtd-process-inbox)
         ("M-<f12> c" . org-gtd-clarify-item)
         ("M-<f12> C" . org-gtd-clarify-agenda-item)
         ("M-<f12> r" . org-gtd-review-area-of-focus)
         ("M-<f12> s" . org-gtd-review-stuck-projects)
         (:map org-gtd-clarify-map
               ("C-c C-c" . org-gtd-organize))))

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

  (setq diary-file (expand-file-name "logs/diary.org" my-galaxy)))

(use-package alert
  :load-path "packages/alert/"
  :commands alert
  :config
  (setq alert-default-style 'osx-notifier))

(defun my/alert-osx-notifier-notify (info)
  (do-applescript (format "display notification %S with title %S"
                          (plist-get info :message)
                          (plist-get info :title)))
  (alert-message-notify info))
(advice-add 'alert-osx-notifier-notify :override #'my/alert-osx-notifier-notify)

(use-package org-alert
  :load-path "packages/org-alert/"
  :hook (org-mode . org-alert-enable)
  :config
  (setq org-alert-interval 300)
  (setq org-alert-notify-cutoff 10)
  (setq org-alert-notify-after-event-cutoff 10)
  (setq org-alert-notification-title "Org Agenda Reminder!")
  (org-alert-enable))

(provide 'init-gtd)
;;; init-gtd.el ends here.
