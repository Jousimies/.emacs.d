;; init-gtd.el --- Tasks management. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(with-eval-after-load 'org
  ;; (setopt org-todo-state-tags-triggers
  ;;         (quote (("CNCL" ("CNCL" . t))
  ;;                 ("WAIT" ("WAIT" . t))
  ;;                 (done ("WAIT"))
  ;;                 ("TODO" ("WAIT") ("CNCL"))
  ;;                 ("NEXT" ("WAIT") ("CNCL"))
  ;;                 ("DONE" ("WAIT") ("CNCL")))))
  (setq org-todo-repeat-to-state t)
  (setq org-todo-keywords
        '((sequence "NEXT(n)" "TODO(t)" "|" "WAIT(w@)" "CNCL(c@/!)" "DONE(d)"))))

(with-eval-after-load 'org-faces
  (setq org-todo-keyword-faces
        '(("TODO" . (:inherit (bold org-todo)))
          ("NEXT" . (:inherit (success org-todo)))
          ("CNCL" . (:inherit (shadow org-todo)))
          ("DONE" . (:inherit (button org-todo)))
          ("WAIT" . (:inherit (warning org-todo)))))
  (setq org-priority-faces
        '((?A . (bold . org-priority))
          (?B . org-priority)
          (?C . (shadow . org-priority)))))

(use-package appt
  :ensure nil
  :hook (diary-mode . appt-activate)
  :custom
  (appt-display-diary nil)
  (appt-disp-window-function #'appt-disp-window)
  (appt-display-mode-line t)
  (appt-display-interval 3)
  (appt-audible nil)
  (appt-warning-time-regexp "appt \\([0-9]+\\)")
  (appt-message-warning-time 6))

(use-package org-agenda
  :ensure nil
  :hook (org-agenda-finalize . org-agenda-to-appt)
  :custom
  (org-agenda-window-setup 'other-tab))

(use-package diary-lib
  :ensure nil
  :hook ((diary-list-entries . diary-sort-entries)
		 (diary-mode . goto-address-mode))
  :custom
  (diary-display-function #'diary-fancy-display)
  (diary-header-line-format nil)
  (diary-list-include-blanks nil)
  (diary-abbreviated-year-flag nil)
  (diary-number-of-entries 7)
  (diary-comment-start ");;")
  (diary-comment-end "")
  (diary-nonmarking-symbol "!")
  (diary-file (expand-file-name "logs/diary.org" my-galaxy)))

(defun my/all-todo-agenda ()
  (interactive)
  (org-agenda nil "n"))

(defun my/book-agenda ()
  (interactive)
  (org-agenda nil "b"))

(with-eval-after-load 'org-agenda
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-todo-ignore-scheduled 'future)
  (setq org-agenda-todo-ignore-deadlines 'near)
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-align-tags-to-column 120)
  (setq org-deadline-warning-days 7)
  (add-to-list 'org-agenda-custom-commands
			   '("b" "Book Shelf"
				 ((tags "+BookShelf"
						((org-agenda-prefix-format " %i")
						 (org-agenda-overriding-header "Reading Lists")))))))

(use-package org-gtd
  :bind (:map org-gtd-clarify-map
              ("C-c C-c" . org-gtd-organize))
  :hook ((org-agenda-mode . org-gtd-mode)
		 (org-mode . org-edna-mode))
  :commands (org-gtd-oops
			 org-gtd-review-area-of-focus
			 org-gtd-review-stuck-projects
			 org-gtd-review-stuck-calendar-items
			 org-gtd-review-stuck-single-action-items
			 org-gtd-review-stuck-delegated-items
			 org-gtd-review-stuck-incubated-items
			 org-gtd-review-stuck-habit-items)
  :init
  (setq org-gtd-update-ack "3.0.0")
  :preface
  (defun my/org-gtd-engage ()
	"Display `org-agenda' customized by org-gtd."
	(interactive)
	(org-gtd-core-prepare-agenda-buffers)
	(with-org-gtd-context
		(let* ((project-format-prefix
				(format " %%i %%-%d:(org-gtd-agenda--prefix-format) "
						org-gtd-engage-prefix-width))
               (org-agenda-custom-commands
				`(("g" "Scheduled today and all NEXT items"
                   ((agenda "" ((org-agenda-span 1)
								(org-deadline-warning-days 0)
								(org-agenda-block-separator nil)
								(org-agenda-skip-additional-timestamps-same-entry t)))
					(tags-todo "*"
							   ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
								(org-agenda-skip-function '(org-agenda-skip-entry-if
															'notregexp org-priority-regexp))
								(org-agenda-block-separator nil)
								(org-agenda-prefix-format '((tags . ,project-format-prefix)))
								(org-agenda-sorting-strategy '(priority-down))
								(org-agenda-overriding-header "Priority Tasks")))
					(todo org-gtd-next
						  ((org-agenda-skip-function '(org-agenda-skip-entry-if
							                           'regexp org-priority-regexp))
						   (org-agenda-overriding-header "All actions ready to be executed.")
						   (org-agenda-sorting-strategy '(category-up tag-up))
                           (org-agenda-prefix-format '((todo . ,project-format-prefix))))))))))
          (org-agenda nil "g")
          (goto-char (point-min)))))
  :custom
  (org-gtd-directory (expand-file-name "iCloud~com~appsonthemove~beorg/Documents/org" icloud))
  (org-agenda-property-list '("DELEGATED_TO"))
  (org-gtd-organize-hooks '(org-gtd-set-area-of-focus org-set-tags-command))
  ;; only archive knowledge things inside `org-gtd-organize.el'.
  (org-gtd-archive-file-format "flash_thoughts_%s")
  (org-edna-use-inheritance t)
  (org-gtd-areas-of-focus '(
							"Basic Life" ;衣食住行方面的基本需求
							"Reading" "Skill" "Finances"	;个人成长方面
							"Hobbies" "Travel" "Amusement" ;Life for amusement and entertainment
							"Health" "Fitness" "Mental" "Spirituality"			  ;Health
							"Spouse" "Kids" "Parents" "Sister" "Relative" ;Family
							"Work" "Professional" "Social"						  ;Work
							))
  (org-gtd-engage-prefix-width 24)
  (org-gtd-clarify-show-horizons 'right)
  :config
  (with-eval-after-load 'org-archive
	;; use `org-archive-subtree' to archive done todos.
	(setq org-archive-location
		  (concat
		   (expand-file-name
			(format-time-string "gtd_archive_%Y") org-gtd-directory)
		   "::datetree/"))))

(use-package alert
  :custom
  (alert-default-style 'osx-notifier)
  :config
  (defun my/alert-osx-notifier-notify (info)
	(do-applescript (format "display notification %S with title %S"
							(plist-get info :message)
							(plist-get info :title)))
	(alert-message-notify info))
  (advice-add 'alert-osx-notifier-notify :override #'my/alert-osx-notifier-notify))

(defun org-clock--get-entries (file)
  (with-current-buffer (find-file-noselect file)
	(let* ((ast (org-element-parse-buffer)))
	  (org-element-map ast 'clock
		(lambda (c) (org-clock--parse-element c))
		nil nil))))

(defun org-clock--find-headlines (element)
  "Returns a list of headline ancestors from closest parent to the farthest"
  (let ((ph (org-element-lineage element '(headline))))
    (if ph
		(cons ph (org-clock--find-headlines ph)))))

(defun org-clock--parse-element (element)
  (let* ((timestamp (org-element-property :value element))
		 (headline (org-clock--find-headlines element))
		 (headline-values (mapcar (lambda (h) (org-element-property :raw-value h)) headline))
		 (start (format "%s %s %s %s:%s"
						(org-element-property :month-start timestamp)
						(org-element-property :day-start timestamp)
						(org-element-property :year-start timestamp)
						(org-element-property :hour-start timestamp)
						(org-element-property :minute-start timestamp)))
		 (end (format "%s %s %s %s:%s"
					  (org-element-property :month-end timestamp)
					  (org-element-property :day-end timestamp)
					  (org-element-property :year-end timestamp)
					  (org-element-property :hour-end timestamp)
					  (org-element-property :minute-end timestamp))))
	(list :headline headline-values
		  :start start
		  :end end)))

(defun my/get-org-clock-files ()
  "Return a list of org files for clock entries."
  (list (concat org-gtd-directory "/" org-gtd-default-file-name ".org")
        (concat my-galaxy "/logs/" (format-time-string "work_log_%Y") ".org")))

(defun my/create-applescript (start end summary)
  "Create an AppleScript to add an event to Calendar."
  (format
   "tell application \"Calendar\"
      tell calendar \"Clocking\"
        set theCurrentDate to date \"%s\"
        set EndDate to date \"%s\"

        set eventExists to false
        repeat with eachEvent in every event
          if start date of eachEvent is theCurrentDate and end date of eachEvent is EndDate then
            set eventExists to true
            exit repeat
          end if
        end repeat

        if not eventExists then
          make new event at end with properties {description:\"\", summary:\"%s\", start date:theCurrentDate, end date:EndDate}
        end if
      end tell
      reload calendars
    end tell"
   start end summary))

(defun my/process-org-clock-entry (entry)
  "Process a single org clock ENTRY and add it to Calendar."
  (let* ((current (calendar-current-date))
         (start (plist-get entry :start))
         (end (plist-get entry :end))
         (headline (plist-get entry :headline))
         (summary (mapconcat 'identity (butlast headline) "-"))
         (apple-script (my/create-applescript start end summary)))
    (when (string-match (format "%s %s %s" (nth 0 current) (nth 1 current) (nth 2 current)) start)
      (unless (featurep 'async)
        (require 'async))
      (async-start
       `(lambda ()
          (shell-command-to-string (format "osascript -e %s" (shell-quote-argument ,apple-script))))
       (lambda (_)
         (message "Async execution completed."))))))

(defun my/org-clock-to-calendar ()
  "Main function to sync org clock entries to Calendar."
  (interactive)
  (unless (featurep 'org-gtd)
    (require 'org-gtd))
  (dolist (file (my/get-org-clock-files))
    (dolist (entry (org-clock--get-entries file))
      (my/process-org-clock-entry entry))))

(add-hook 'org-clock-out-hook #'my/org-clock-to-calendar)

(defun org-headline-contains-clock-info ()
  "Check if the current headline contains clock information."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (outline-next-heading) (point)))
          (clock-regexp org-clock-line-re))
      (re-search-forward clock-regexp end t))))

(defun my/done-task-to-calendar ()
  (interactive)
  (let* ((timestamp (org-element-map (org-element-at-point) 'headline
					  (lambda (headline)
						(org-element-property :closed headline))
					  nil
					  t))
		 (state (org-element-property :todo-type (org-element-at-point)))
		 (start (format "%s %s %s %s:%s"
						(org-element-property :month-start timestamp)
						(org-element-property :day-start timestamp)
						(org-element-property :year-start timestamp)
						(org-element-property :hour-start timestamp)
						(org-element-property :minute-start timestamp)))
		 (headline (concat
					(org-element-property :todo-keyword (org-element-at-point))
					" "
					(org-element-property :raw-value (org-element-at-point))))
		 (apple-script (format
						"tell application \"Calendar\"
                             tell calendar \"Clocking\"
                               set theCurrentDate to date \"%s\"
                               set EndDate to theCurrentDate + 2

                               make new event at end with properties {description:\"\", summary:\"%s\", start date:theCurrentDate, end date:EndDate}

                             end tell
                             reload calendars
                           end tell"
						start headline)))
	(unless (featurep 'async)
	  (require 'async))
	(when (and (eq state 'done)
			   (not (org-headline-contains-clock-info)))
	  (async-start
       `(lambda ()
          (shell-command-to-string (format "osascript -e %s" (shell-quote-argument ,apple-script))))
       (lambda (_)
		 (message "Async execution completed."))))))

(add-hook 'org-after-todo-state-change-hook #'my/done-task-to-calendar)


(provide 'init-gtd)
;;; init-gtd.el ends here.
