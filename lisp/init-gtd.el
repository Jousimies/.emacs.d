;; init-gtd.el --- Tasks management. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(defun my/all-todo-agenda ()
  (interactive)
  (org-agenda nil "n"))

(defun my/book-agenda ()
  (interactive)
  (org-agenda nil "b"))

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
		    (tags-todo "项目"
			       ())
		    (todo org-gtd-next
			  ((org-agenda-skip-function '(org-agenda-skip-entry-if
						       'regexp org-priority-regexp))
			   (org-agenda-overriding-header "All actions ready to be executed.")
			   (org-agenda-sorting-strategy '(category-up tag-up))
                           (org-agenda-prefix-format '((todo . ,project-format-prefix)))))
		    )))))
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
  "获取当前 org heading 的标题和内容。
如果存在一级标题，则返回 '一级标题 / 当前标题' 作为 summary。"
  (let* ((element (org-element-at-point))
         (heading (org-no-properties (org-get-heading t t t t)))
         (parent (org-element-property :parent element))
         (summary heading))
    ;; 查找上级 headline（如果有）
    (while (and parent (not (eq (org-element-type parent) 'headline)))
      (setq parent (org-element-property :parent parent)))
    (when (and parent (= (org-element-property :level parent) 2))
      (setq summary
            (format "%s/%s"
                    (org-no-properties
                     (org-element-property :raw-value parent))
                    heading)))
    ;; 提取 clock 内容
    (let ((clocklog
           (buffer-substring-no-properties
            (org-element-property :contents-begin element)
            (org-element-property :contents-end element))))
      (list summary clocklog))))


;; (defun org2calendar-get-heading-and-clocklog ()
;;   "获取当前 org heading 的标题和内容。"
;;   (let ((heading (org-no-properties (org-get-heading t t t t)))
;;         (clocklog (buffer-substring-no-properties
;;                    (org-element-property :contents-begin (org-element-headline-parser))
;;                    (org-element-property :contents-end (org-element-headline-parser)))))
;;     (list heading clocklog)))

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
    (if (display-graphic-p)
	(alert (format "Start Sync to Calendar: %s" summary))
      (message (format "Start Sync to Calendar: %s" summary)))
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

;; Dynamic agenda
(defun ad/agenda-file-p ()
  (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
    (lambda (h)
      (eq (org-element-property :todo-type h)
          'todo))
    nil 'first-match))

(defun ad/org-agenda-update-files (&rest ARG)
  ;; check if this is an org file buffer
  (interactive)
  (when (and (derived-mode-p 'org-mode) (buffer-file-name))
    (message "updating org-agenda-files...")
    ;; if there is an active TODO task, add this file to agenda files
    (if (ad/agenda-file-p)
	(add-to-list 'org-agenda-files (file-truename (buffer-file-name)))
      ;; if there is no active TODO task, remove the file from agenda files if needed
      (setq org-agenda-files (seq-difference org-agenda-files (list (buffer-file-name))))
      (customize-save-variable 'org-agenda-files org-agenda-files)
      )))

(defun ad/org-agenda-cleanup-files (&rest ARG)
  (interactive)
  (let ((temp/org-agenda-files org-agenda-files))
    (dolist (file org-agenda-files)
      (if (not (file-exists-p file))
	  (setq temp/org-agenda-files (seq-difference temp/org-agenda-files (list file))))
      ())
    (setq org-agenda-files temp/org-agenda-files))
  )

;; Add or remove individual file
(add-hook 'org-mode-hook (lambda () (add-hook 'find-file-hook #'ad/org-agenda-update-files)))
(add-hook 'org-mode-hook (lambda () (add-hook 'before-save-hook #'ad/org-agenda-update-files)))

;; remove non-existing files before building agenda
(advice-add 'org-agenda :before #'ad/org-agenda-cleanup-files)
(advice-add 'org-todo-list :before #'ad/org-agenda-cleanup-files)
(advice-add 'org-gtd-engage :before #'ad/org-agenda-cleanup-files)

(when (featurep 'savehist)
  (add-to-list 'savehist-additional-variables 'org-agenda-files))

(provide 'init-gtd)
;;; init-gtd.el ends here.
