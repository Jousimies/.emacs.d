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
    "/" 'org-agenda-filter
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

;;;###autoload
(defun my/gtd-file ()
  (interactive)
  (find-file (expand-file-name "todos/org-gtd-tasks.org" my-galaxy)))

(use-package org-gtd
  :commands org-gtd-capture org-gtd-engage org-gtd-process-inbox org-gtd-show-all-next org-gtd-show-stuck-projects
  :bind (("<f12>" . org-gtd-engage)
         (:map org-gtd-process-map
               ("C-c c" . org-gtd-choose)))
  :init
  (setq org-gtd-update-ack "2.1.0")
  :general (my/space-leader-def
             "d" '(:ignore t :wk "Org gtd")
             "dp" '(org-gtd-process-inbox :wk "Process")
             "da" '(org-agenda-list :wk "Agenda list")
             "de" '(org-gtd-engage :wk "Engage")
             "dn" '(org-gtd-show-all-next :wk "Next tasks")
             "ds" '(org-gtd-show-stuck-projects :wk "Stuck projects"))
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
  :general (my/space-leader-def
             "c" '(calendar :wk "Calendar"))
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

(provide 'init-gtd)
;;; init-gtd.el ends here.
