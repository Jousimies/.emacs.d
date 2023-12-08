;; init-gtd.el --- Tasks management. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:
(with-eval-after-load 'calendar
  (setq calendar-view-diary-initially-flag t)
  (setq calendar-mark-diary-entries-flag t)

  (setq calendar-date-style 'iso)
  (setq calendar-date-display-form calendar-iso-date-display-form)
  (setq diary-date-forms diary-iso-date-forms)
  (setq calendar-time-display-form
        '(24-hours ":" minutes
                   (when time-zone
                     (format "(%s)" time-zone))))
  (add-hook 'calendar-today-visible-hook #'calendar-mark-today))

(global-set-key (kbd "C-c C") #'calendar)

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

(global-set-key (kbd "C-<F12>") #'org-agenda)

(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-finalize-hook #'org-agenda-find-same-or-today-or-agenda)
  (setq org-agenda-window-setup 'other-tab)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-todo-ignore-scheduled 'future)
  (setq org-agenda-todo-ignore-deadlines 'near)
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-align-tags-to-column 'auto)
  (setq org-deadline-warning-days 7)
  (add-to-list 'org-agenda-custom-commands
			   '("b" "Book Shelf"
				 ((tags "+BookShelf"
						((org-agenda-prefix-format " %i")
						 (org-agenda-overriding-header "Reading Lists")))))))

(with-eval-after-load 'org-archive
  (setq org-archive-location "%s_archive::datetree/"))

;; Refile done todo to denote daily journal.
(with-eval-after-load 'denote
  (defun my/org-refile-on-todo-done ()
    "Refile a task to a different file when it is marked as DONE."
    (let ((org-refile-keep t))
      (when (string= org-state "DONE")
        (org-refile nil nil (list nil (car (denote-journal-extras--entry-today))) "Copy"))))
  (add-hook 'org-after-todo-state-change-hook 'my/org-refile-on-todo-done))

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

  (add-hook 'org-mode-hook #'org-edna-mode)
  :bind (("<f12>" . org-gtd-engage)
         ("s-<f12>" . org-gtd-process-inbox)
         ("M-<f12> c" . org-gtd-clarify-item)
         ("M-<f12> C" . org-gtd-clarify-agenda-item)
         ("M-<f12> r" . org-gtd-review-area-of-focus)
         ("M-<f12> s" . org-gtd-review-stuck-projects)
         (:map org-gtd-clarify-map
               ("C-c C-c" . org-gtd-organize))))

;; org pomodoro
;; TODO add to modeline
(use-package hammy
  :load-path "packages/hammy.el/" "packages/svg-lib" "packages/ts.el"
  :commands hammy-start
  :bind (("<f9>" . hammy-start)
         ("C-<f9>" . hammy-stop))
  :hook (hammy-start . hammy-mode)
  :custom
  (hammy-mode-always-show-lighter nil)
  (hammy-mode-lighter-prefix "")
  (hammy-mode-lighter-pie nil)
  :config
  ;; https://github.com/alphapapa/hammy.el/issues/10
  ;; Use alert instead of notifications-notify, which do not work on MacOS platform.
  (define-advice notifications-notify
      (:override (&rest params) using-alert)
    (alert (plist-get params :body)
           :title (plist-get params :title))))

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

(provide 'init-gtd)
;;; init-gtd.el ends here.
