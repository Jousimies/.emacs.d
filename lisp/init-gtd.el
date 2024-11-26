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

(defun my/all-todo-agenda ()
  (interactive)
  (org-agenda nil "n"))

(defun my/book-agenda ()
  (interactive)
  (org-agenda nil "b"))

(with-eval-after-load 'org-agenda
  (setq org-agenda-window-setup 'other-tab)
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
  :load-path ("packages/org-gtd.el/" "packages/org-agenda-property" "packages/org-edna")
  :commands (org-gtd-process-inbox
			 org-gtd-engage-grouped-by-context
			 org-gtd-engage
			 org-gtd-clarify-item
			 org-gtd-clarify-agenda-item
			 org-gtd-oops
			 org-gtd-review-area-of-focus
			 org-gtd-review-stuck-projects
			 org-gtd-review-stuck-calendar-items
			 org-gtd-review-stuck-single-action-items
			 org-gtd-review-stuck-delegated-items
			 org-gtd-review-stuck-incubated-items
			 org-gtd-review-stuck-habit-items)
  :init
  (setq org-gtd-update-ack "3.0.0")
  :hook ((org-agenda-mode . org-gtd-mode)
		 (org-mode . org-edna-mode))
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
  :bind ((:map org-gtd-clarify-map
               ("C-c C-c" . org-gtd-organize)))
  :config
  (with-eval-after-load 'org-archive
	;; use `org-archive-subtree' to archive done todos.
	(setq org-archive-location
		  (concat
		   (expand-file-name
			(format-time-string "gtd_archive_%Y") org-gtd-directory)
		   "::datetree/"))))

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

;; Sync org entry with clocking to MacOS Calendar.
(defun org-clock--find-headlines (element)
  "Returns a list of headline ancestors from closest parent to the farthest"
  (let ((ph (org-element-lineage element '(headline))))
    (if ph
		(cons ph (org-clock--find-headlines ph)))))

(defun org-clock--parse-element (element)
  (let* ((timestamp (org-element-property :value element))
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
	(list :start start
		  :end end)))

(defun my/create-applescript (start end summary)
  "Create an AppleScript to add an event to Calendar."
  (format
   "tell application \"Calendar\"
      tell calendar \"Clocking\"
        set theCurrentDate to date \"%s\"
        set EndDate to date \"%s\"
        make new event at end with properties {description:\"\", summary:\"%s\", start date:theCurrentDate, end date:EndDate}
      end tell
      reload calendars
    end tell"
   start end summary))

(defun org2calendar-get-heading-and-clocklog ()
  "获取当前 org heading 的标题和内容。"
  (let ((heading (org-no-properties (org-get-heading t t t t)))
        (clocklog (buffer-substring-no-properties
                   (org-element-property :contents-begin (org-element-headline-parser))
                   (org-element-property :contents-end (org-element-headline-parser)))))
    (list heading clocklog)))

(defun org2calendar-extract-clock-entries (clocklog)
  "从 Org 内容中提取 CLOCK 记录。"
  (with-temp-buffer
    (insert clocklog)
    (let* ((ast (org-element-parse-buffer)))
      (org-element-map ast 'clock
        (lambda (c) (org-clock--parse-element c))
        nil nil))))

(defun org2calendar-sync (start end summary)
  "生成 AppleScript 并异步执行它。"
  (let ((apple-script (my/create-applescript start end summary)))
    (alert (format "Start Sync to Calendar: %s" summary))
	(unless (featurep 'async)
	  (require 'async))
    (async-start
     `(lambda ()
        (shell-command-to-string (format "osascript -e %s" (shell-quote-argument ,apple-script))))
     (lambda (_)
       (message "Async execution completed.")))))

(defun org2calendar-handle-last-clock-entry ()
  "处理当前 org-clock-history 中的 CLOCK 记录。"
  (interactive)
  (with-current-buffer (marker-buffer (car org-clock-history))
    (goto-char (car org-clock-history))
    (org-back-to-heading)
    (let* ((heading-and-clocklog (org2calendar-get-heading-and-clocklog))
           (heading (nth 0 heading-and-clocklog))
           (clock-entries (org2calendar-extract-clock-entries (nth 1 heading-and-clocklog)))
           (entry (nth 0 clock-entries))
           (start (plist-get entry :start))
           (end (plist-get entry :end)))
      (org2calendar-sync start end heading))))

(add-hook 'org-clock-out-hook #'org2calendar-handle-last-clock-entry)

(defun org2calendar-handle-current-clock-entry ()
  "处理当前条目中的 CLOCK 记录。"
  (interactive)
  (with-current-buffer (marker-buffer (car org-clock-history))
    (org-back-to-heading)
    (let* ((heading-and-clocklog (org2calendar-get-heading-and-clocklog))
           (heading (nth 0 heading-and-clocklog))
           (clock-entries (org2calendar-extract-clock-entries (nth 1 heading-and-clocklog)))
           (entry (nth 0 clock-entries))
           (start (plist-get entry :start))
           (end (plist-get entry :end)))
      (org2calendar-sync start end heading))))


(provide 'init-gtd)
;;; init-gtd.el ends here.
