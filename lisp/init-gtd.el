;; init-gtd.el --- GTD system*- lexical-binding: t; no-byte-compile: t -*-

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
  (setq org-agenda-align-tags-to-column -120)

  (with-eval-after-load 'all-the-icons
    (setq org-agenda-category-icon-alist
          `(("\\`gtd\\'"
             (#(" " 0 1 (rear-nonsticky t display (raise 0.0)
                                         font-lock-face
                                         (:family "FontAwesome" :height 1.0)
                                         face
                                         (:family "FontAwesome" :height 1.0))))
             nil nil :ascent center)
            ("\\\cc\\\|[a-zA-z0-9]*"
             (#(" " 0 1 (rear-nonsticky t display (raise 0.0)
                                         font-lock-face
                                         (:family "FontAwesome" :height 1.0)
                                         face
                                         (:family "FontAwesome" :height 1.0))))
             nil nil :ascent center)))))

(with-eval-after-load 'evil
  (evil-set-initial-state 'org-agenda-mode 'motion)
  (evil-define-key 'motion org-agenda-mode-map
    (kbd "RET") 'org-agenda-switch-to
    "SPC" 'nil
    "gj" 'org-agenda-next-item
    "gr" 'org-agenda-redo
    "gR" 'org-agenda-redo-all
    "t" 'org-agenda-todo
    "u" 'org-agenda-undo
    "I" 'org-agenda-clock-in
    "O" 'org-agenda-clock-out
    "cg" 'org-agenda-clock-goto
    "cc" 'org-agenda-clock-cancel
    "cr" 'org-agenda-clockreport-mode))

(defun my/gtd-file ()
  (interactive)
  (find-file (expand-file-name "todos/org-gtd-tasks.org" my-galaxy)))

(use-package org-gtd
  :bind ("<f12>" . org-gtd-engage)
  :commands org-gtd-capture org-gtd-engage org-gtd-process-inbox org-gtd-show-all-next org-gtd-show-stuck-projects
  :init
  (setq org-gtd-update-ack "2.1.0")
  :custom
  (org-gtd-directory (expand-file-name "todos" my-galaxy))
  (org-agenda-property-list '("DELEGATED_TO"))
  (org-edna-use-inheritance t)
  :config
  (add-to-list 'org-agenda-files (expand-file-name "todos/org-gtd-tasks.org" my-galaxy))
  :bind (:map org-gtd-process-map
          ("C-c c" . org-gtd-choose)))

(my/space-leader-def
  "d" '(:ignore t :wk "Org gtd")
  "dp" '(org-gtd-process-inbox :wk "Process")
  "da" '(org-agenda-list :wk "Agenda list")
  "de" '(org-gtd-engage :wk "Engage")
  "dn" '(org-gtd-show-all-next :wk "Next tasks")
  "ds" '(org-gtd-show-stuck-projects :wk "Stuck projects"))

(use-package org-edna
  :after org-gtd
  :config
  (org-edna-load))

(use-package calendar
  :commands calendar
  :config
  (setq calendar-view-diary-initially-flag t)
  (setq calendar-mark-diary-entries-flag t)
  (setq calendar-mode-line-format nil)

  (setq calendar-date-style 'iso)
  (setq calendar-date-display-form calendar-iso-date-display-form)

  (setq calendar-time-display-form
        '(24-hours ":" minutes
                   (when time-zone
                     (format "(%s)" time-zone))))
  (setq diary-date-forms diary-iso-date-forms)
  :hook (calendar-today-visible . #'calendar-mark-today))

(my/space-leader-def
  "c" '(calendar :wk "Calendar"))

(use-package appt
  :after calendar
  :config
  (setq appt-display-diary nil)
  (setq appt-disp-window-function #'appt-disp-window)
  (setq appt-display-mode-line t)
  (setq appt-display-interval 3)
  (setq appt-audible nil)
  (setq appt-warning-time-regexp "appt \\([0-9]+\\)")
  (setq appt-message-warning-time 6)
  (add-hook 'diary-mode-hook #'appt-activate))

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

(use-package alarm-clock
  :commands (alarm-clock-set alarm-clock-list-view)
  :config
  (setq alarm-clock-cache-file (expand-file-name "var/.alarm-clock.cache" user-emacs-directory)))

(use-package alert
  :commands alert
  :config
  (setq alert-default-style 'osx-notifier))

(use-package org-alert
  :hook (on-first-file . org-alert-enable)
  :config
  (setq org-alert-interval 300)
  (setq org-alert-notify-cutoff 10)
  (setq org-alert-notify-after-event-cutoff 10)
  (setq org-alert-notification-title "Org Agenda Reminder!"))

(use-package pomm
  :commands pomm
  :config
  (setq pomm-state-file-location (expand-file-name "cache/pomm" user-emacs-directory))
  (pomm-mode-line-mode 1))

(provide 'init-gtd)
;;; init-gtd.el ends here.