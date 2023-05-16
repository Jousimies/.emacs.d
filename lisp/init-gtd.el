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

(use-package org-gtd
  ;; :commands org-gtd-capture org-gtd-engage org-gtd-process-inbox org-gtd-show-all-next org-gtd-show-stuck-projects
  :bind (("<f12>" . org-gtd-engage)
         ("C-<f12>" . org-gtd-process-inbox)
         ("s-<f12>" . org-gtd-show-stuck-projects)
         (:map org-gtd-clarify-map
               ("C-c c" . org-gtd-organize)))
  :init
  (setq org-gtd-update-ack "3.0.0")
  :custom
  (org-gtd-directory (expand-file-name "todos" my-galaxy))
  (org-agenda-property-list '("DELEGATED_TO"))
  (org-gtd-organize-hooks '(org-gtd-set-area-of-focus org-set-tags-command))
  :config
  (add-to-list 'org-agenda-files (expand-file-name "todos/org-gtd-tasks.org" my-galaxy)))

(use-package org-edna
  :after org-gtd
  :config
  (setq org-edna-use-inheritance t)
  (org-edna-load))

(use-package vulpea
  :after org-roam
  :config
  (defun vulpea-project-p ()
    "Return non-nil if current buffer has any todo entry.
  TODO entries marked as done are ignored, meaning the this
  function returns nil if current buffer contains only completed
  tasks."
    (seq-find                                 ; (3)
     (lambda (type)

       (or (eq type 'todo)
           (eq type 'done)))
     (org-element-map                         ; (2)
         (org-element-parse-buffer 'headline) ; (1)
         'headline
       (lambda (h)
         (org-element-property :todo-type h)))))

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun vulpea-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-project-p)
              (setq tags (cons "project" tags))
            (setq tags (remove "project" tags)))
          ;; cleanup duplicates
          (setq tags (seq-uniq tags))
          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

  (defun vulpea-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
                :from tags
                :left-join nodes
                :on (= tags:node-id nodes:id)
                :where (like tag (quote "%\"project\"%"))]))))

  (defun vulpea-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (seq-uniq
                            (append
                             (vulpea-project-files)
                             `(,(expand-file-name "todos/org-gtd-tasks.org" my-galaxy))))))

  (add-hook 'find-file-hook #'vulpea-agenda-files-update)

  (advice-add 'org-agenda :before #'vulpea-agenda-files-update)
  (advice-add 'org-todo-list :before #'vulpea-agenda-files-update)

  (add-hook 'find-file-hook #'vulpea-project-update-tag)
  (add-hook 'before-save-hook #'vulpea-project-update-tag))

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
