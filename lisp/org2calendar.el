;;; org2calendar.el --- Org agenda bridge for org2calendar -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:

;;; defaults write /Applications/Emacs.app/Contents/Info.plist NSRemindersFullAccessUsageDescription "Org-mode 需要访问提醒事项以同步任务清单。"

;; Export the tasks already selected by an Org agenda to Apple Reminders.
;; The native module must be loaded before calling the interactive command.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(require 'org-id)

(defgroup org2calendar nil
  "Synchronize Org tasks with Calendar and Reminders."
  :group 'org)

(defcustom org2calendar-reminder-list "Work"
  "Apple Reminders list used by `org2calendar-sync-agenda-tasks'."
  :type 'string
  :group 'org2calendar)

(defcustom org2calendar-reminder-lookback-days 30
  "Number of days queried by `org2calendar-pull-reminder-completions'."
  :type 'integer
  :group 'org2calendar)

(defcustom org2calendar-auto-pull-idle-seconds 300
  "Idle seconds between automatic Reminder completion pull requests."
  :type 'number
  :group 'org2calendar)

(defcustom org2calendar-auto-pull-debounce-seconds 30
  "Minimum seconds between completed automatic Reminder pulls."
  :type 'number
  :group 'org2calendar)

(defvar org2calendar--auto-pull-idle-timer nil)
(defvar org2calendar--auto-pull-pending-timer nil)
(defvar org2calendar--auto-pull-running nil)
(defvar org2calendar--auto-pull-last-finished-at nil)

(defconst org2calendar--project-agenda-buffer-name
  "*Org2Calendar Project Tasks*"
  "Buffer used to select project tasks for Reminder export.")

(cl-defstruct (org2calendar-sync-summary
               (:constructor org2calendar--make-sync-summary))
  created
  updated
  skipped
  failed
  missing-id
  details)

(cl-defstruct (org2calendar-reminder-pull-summary
               (:constructor org2calendar--make-reminder-pull-summary))
  completed
  skipped
  failed
  fallback-time
  details)

(defun org2calendar--new-sync-summary ()
  "Return an empty `org2calendar-sync-summary'."
  (org2calendar--make-sync-summary
   :created 0 :updated 0 :skipped 0 :failed 0 :missing-id 0 :details nil))

(defun org2calendar--new-reminder-pull-summary ()
  "Return an empty `org2calendar-reminder-pull-summary'."
  (org2calendar--make-reminder-pull-summary
   :completed 0 :skipped 0 :failed 0 :fallback-time 0 :details nil))

(defun org2calendar--active-keywords ()
  "Return the configured org-gtd keywords eligible for export."
  (if (boundp 'org-gtd-keyword-mapping)
      (delq nil
            (mapcar (lambda (semantic)
                      (alist-get semantic org-gtd-keyword-mapping))
                    '(todo next wait)))
    '("TODO" "NEXT" "WAIT")))

(defun org2calendar--valid-marker-p (marker)
  "Return non-nil when MARKER points into a live buffer."
  (and (markerp marker)
       (marker-buffer marker)
       (buffer-live-p (marker-buffer marker))
       (marker-position marker)))

(defun org2calendar--markers-in-region (begin end)
  "Collect source heading markers from agenda lines between BEGIN and END."
  (let (markers)
    (save-excursion
      (goto-char begin)
      (while (< (point) end)
        (unless (invisible-p (line-beginning-position))
          (when-let* ((marker (or (org-get-at-bol 'org-hd-marker)
                                  (org-get-at-bol 'org-marker))))
            (push marker markers)))
        (forward-line 1)))
    (nreverse markers)))

(defun org2calendar--deduplicate-markers (markers)
  "Return live MARKERS once each, preserving their first-seen order."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (marker markers (nreverse result))
      (when (org2calendar--valid-marker-p marker)
        (let ((key (cons (marker-buffer marker) (marker-position marker))))
          (unless (gethash key seen)
            (puthash key t seen)
            (push marker result)))))))

(defun org2calendar--agenda-markers ()
  "Return task markers selected by the current Org agenda.

Bulk marks take precedence over an active region.  With neither, return every
task on the current agenda line."
  (unless (derived-mode-p 'org-agenda-mode)
    (user-error "请在 org-agenda 中运行 org2calendar 同步"))
  (org2calendar--deduplicate-markers
   (cond
    (org-agenda-bulk-marked-entries
     (reverse org-agenda-bulk-marked-entries))
    ((use-region-p)
     (org2calendar--markers-in-region (region-beginning) (region-end)))
    (t
     (list (or (org-get-at-bol 'org-hd-marker)
               (org-get-at-bol 'org-marker)
               (user-error "光标所在行没有可同步的 Org 任务")))))))

(defun org2calendar--agenda-marker-at-point ()
  "Return the source Org marker on the current agenda line."
  (unless (derived-mode-p 'org-agenda-mode)
    (user-error "请在 org-agenda 中打开项目规划视图"))
  (or (org-get-at-bol 'org-hd-marker)
      (org-get-at-bol 'org-marker)
      (user-error "光标所在行没有 Org 任务")))

(defun org2calendar--project-candidates (task-marker)
  "Return project display-name and marker pairs for TASK-MARKER."
  (org-with-point-at task-marker
    (save-restriction
      (widen)
      (org-back-to-heading t)
      (let* ((entry-id (org-entry-get nil "ID"))
             (project-ids
              (or (org-entry-get-multivalued-property
                   nil "ORG_GTD_PROJECT_IDS")
                  (and entry-id
                       (equal (org-entry-get nil "ORG_GTD") "Projects")
                       (list entry-id))))
             candidates)
        (dolist (project-id project-ids (nreverse candidates))
          (when-let* ((marker (org-id-find project-id t))
                      ((org2calendar--valid-marker-p marker)))
            (push
             (cons
              (org-with-point-at marker
                (format "%s [%s]"
                        (org-get-heading t t t t)
                        project-id))
              marker)
             candidates)))))))

(defun org2calendar--project-marker-at-agenda-point ()
  "Resolve the org-gtd project for the current agenda task."
  (let ((candidates
         (org2calendar--project-candidates
          (org2calendar--agenda-marker-at-point))))
    (pcase candidates
      ('() (user-error "当前任务不属于可定位的 org-gtd 项目"))
      (`((,_ . ,marker)) marker)
      (_
       (let* ((choice
               (completing-read
                "选择要规划的项目: " (mapcar #'car candidates) nil t))
              (marker (cdr (assoc choice candidates))))
         (or marker (user-error "未选择有效项目")))))))

(defun org2calendar--active-project-task-markers (project-marker)
  "Return active task markers belonging to PROJECT-MARKER.

Use org-gtd's dependency graph traversal so blocked and cross-file tasks are
included instead of relying on the current agenda's skip function."
  (unless (fboundp 'org-gtd-dependencies-collect-project-tasks)
    (require 'org-gtd-dependencies nil t))
  (unless (fboundp 'org-gtd-dependencies-collect-project-tasks)
    (user-error "org-gtd 依赖图接口不可用"))
  (cl-remove-if-not
   (lambda (marker)
     (and
      (org2calendar--valid-marker-p marker)
      (org-with-point-at marker
        (save-restriction
          (widen)
          (org-back-to-heading t)
          (member (org-get-todo-state)
                  (org2calendar--active-keywords))))))
   (org2calendar--deduplicate-markers
    (org-gtd-dependencies-collect-project-tasks project-marker))))

(defun org2calendar--insert-project-task-line (marker)
  "Insert one project planning agenda line for MARKER."
  (let (state title)
    (org-with-point-at marker
      (save-restriction
        (widen)
        (org-back-to-heading t)
        (setq state (org-get-todo-state)
              title (org-get-heading t t t t))))
    (let ((line-start (point))
          (source-marker (copy-marker marker)))
      (insert "  " (propertize state 'face (org-get-todo-face state))
              " " title "\n")
      (add-text-properties
       line-start (1- (point))
       `(org-hd-marker ,source-marker
         org-marker ,source-marker
         todo-state ,state
         org-heading t
         txt ,title)))))

(defun org2calendar--build-project-agenda (project-marker)
  "Build and return a planning agenda for PROJECT-MARKER."
  (let* ((project-title
          (org-with-point-at project-marker
            (org-get-heading t t t t)))
         (task-markers
          (org2calendar--active-project-task-markers project-marker))
         (buffer (get-buffer-create org2calendar--project-agenda-buffer-name)))
    (with-current-buffer buffer
      (org-agenda-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (remove-overlays)
        (setq-local org-agenda-bulk-marked-entries nil)
        (setq-local org-agenda-type 'org2calendar-project)
        (insert (propertize (format "项目规划: %s\n\n" project-title)
                            'face 'org-agenda-structure))
        (dolist (marker task-markers)
          (org2calendar--insert-project-task-line marker))
        (unless task-markers
          (insert "没有未完成的项目任务。\n"))
        (goto-char (point-min))
        (when-let* ((first-task
                     (text-property-not-all
                      (point-min) (point-max) 'org-hd-marker nil)))
          (goto-char first-task))))
    buffer))

;;;###autoload
(defun org2calendar-show-project-tasks ()
  "Show every active task in the project at the current agenda line.

The resulting temporary agenda includes blocked TODO/NEXT/WAIT tasks.  Mark
the tasks planned for today with `org-agenda-bulk-mark', then run
`org2calendar-sync-agenda-tasks'."
  (interactive)
  (pop-to-buffer
   (org2calendar--build-project-agenda
    (org2calendar--project-marker-at-agenda-point))))

(defun org2calendar--entry-title ()
  "Return the Reminder title for the Org entry at point."
  (if (fboundp 'org2calendar-get-context-summary)
      (org2calendar-get-context-summary)
    (org-get-heading t t t t)))

(defun org2calendar--entry-due-string ()
  "Return the entry's scheduled or deadline timestamp for the native module."
  (when-let* ((timestamp (or (org-entry-get nil "SCHEDULED")
                             (org-entry-get nil "DEADLINE"))))
    (let ((time (org-time-string-to-time timestamp)))
      (if (string-match-p "[012][0-9]:[0-5][0-9]" timestamp)
          (format-time-string "%Y-%m-%dT%H:%M:%S%:z" time)
        (format-time-string "%Y-%m-%d" time)))))

(defun org2calendar--record-result (summary title org-id result)
  "Record native RESULT for TITLE and ORG-ID in SUMMARY."
  (pcase result
    ("created" (cl-incf (org2calendar-sync-summary-created summary)))
    ("updated" (cl-incf (org2calendar-sync-summary-updated summary)))
    ((or "skipped" "skipped-done-missing")
     (cl-incf (org2calendar-sync-summary-skipped summary)))
    (_
     (cl-incf (org2calendar-sync-summary-failed summary))))
  (push (list :title title :org-id org-id :result result)
        (org2calendar-sync-summary-details summary)))

(defun org2calendar--sync-marker (marker list-name summary)
  "Sync the Org entry at MARKER to LIST-NAME and update SUMMARY."
  (condition-case error-data
      (org-with-point-at marker
        (save-restriction
          (widen)
          (org-back-to-heading t)
          (let ((state (org-get-todo-state)))
            (if (not (member state (org2calendar--active-keywords)))
                (progn
                  (cl-incf (org2calendar-sync-summary-skipped summary))
                  (push (list :title (org-get-heading t t t t)
                              :result "not-active")
                        (org2calendar-sync-summary-details summary)))
              (let ((org-id (org-entry-get nil "ID"))
                    (title (org2calendar--entry-title)))
                (if (not (and org-id (not (string-empty-p org-id))))
                    (progn
                      (cl-incf (org2calendar-sync-summary-failed summary))
                      (cl-incf (org2calendar-sync-summary-missing-id summary))
                      (push (list :title title :result "missing-org-id")
                            (org2calendar-sync-summary-details summary)))
                  (org2calendar--record-result
                   summary title org-id
                   (org2calendar-sync-reminder
                    title list-name org-id state
                    (or (org2calendar--entry-due-string) "")))))))))
    (error
     (cl-incf (org2calendar-sync-summary-failed summary))
     (push (list :result "elisp-error"
                 :error (error-message-string error-data))
           (org2calendar-sync-summary-details summary)))))

(defun org2calendar--summary-message (summary list-name)
  "Display SUMMARY for LIST-NAME."
  (message
   "[org2calendar → %s] 创建:%d 更新:%d 跳过:%d 失败:%d（缺少 ID:%d）"
   list-name
   (org2calendar-sync-summary-created summary)
   (org2calendar-sync-summary-updated summary)
   (org2calendar-sync-summary-skipped summary)
   (org2calendar-sync-summary-failed summary)
   (org2calendar-sync-summary-missing-id summary)))

(defun org2calendar--done-keyword ()
  "Return the configured done keyword for the current Org buffer."
  (or (and (boundp 'org-gtd-keyword-mapping)
           (alist-get 'done org-gtd-keyword-mapping))
      (car org-done-keywords)
      "DONE"))

(defun org2calendar--iso-time (time)
  "Format TIME for the native completed-reminders query."
  (format-time-string "%Y-%m-%dT%H:%M:%S%:z" time))

(defun org2calendar--reminder-window (&optional start end)
  "Return native query strings for optional START and END times."
  (let ((window-end (or end (current-time))))
    (list (org2calendar--iso-time
           (or start
               (time-subtract
                window-end
                (days-to-time org2calendar-reminder-lookback-days))))
          (org2calendar--iso-time window-end))))

(defun org2calendar--completion-time (value fallback)
  "Parse completion time VALUE, using FALLBACK when VALUE is nil."
  (if value
      (date-to-time value)
    fallback))

(defun org2calendar--marker-has-id-p (marker org-id)
  "Return non-nil when MARKER still points to the heading for ORG-ID."
  (and (org2calendar--valid-marker-p marker)
       (org-with-point-at marker
         (save-restriction
           (widen)
           (org-back-to-heading t)
           (equal (org-entry-get nil "ID") org-id)))))

(defun org2calendar--set-closed-time (org-id original-marker completion-time)
  "Relocate ORG-ID after hooks and set its CLOSED to COMPLETION-TIME.

Use ORIGINAL-MARKER only when the task remains at its original location."
  (let ((marker (org-id-find org-id t)))
    (unless (org2calendar--marker-has-id-p marker org-id)
      (setq marker original-marker))
    (unless (org2calendar--marker-has-id-p marker org-id)
      (error "Org task disappeared after org-todo: %s" org-id))
    (org-with-point-at marker
      (save-restriction
        (widen)
        (org-back-to-heading t)
        (org-add-planning-info 'closed completion-time)))))

(defun org2calendar--repair-gtd-projects (project-ids)
  "Recalculate TODO keywords for every project in PROJECT-IDS.

This is an idempotent postcondition for the org-edna trigger.  It also repairs
projects when org-edna was disabled or its trigger did not run."
  (when project-ids
    (unless (fboundp 'org-gtd-projects-fix-todo-keywords)
      (require 'org-gtd-projects nil t))
    (unless (fboundp 'org-gtd-projects-fix-todo-keywords)
      (error "org-gtd project keyword repair is unavailable"))
    (dolist (project-id (delete-dups (copy-sequence project-ids)))
      (let ((project-marker (org-id-find project-id t)))
        (unless (org2calendar--marker-has-id-p project-marker project-id)
          (error "Org GTD project not found: %s" project-id))
        (org-gtd-projects-fix-todo-keywords project-marker)))))

(defun org2calendar--complete-marker
    (marker org-id completion-time fallback-time)
  "Complete task at MARKER and return its result plist."
  (org-with-point-at marker
    (save-restriction
      (widen)
      (org-back-to-heading t)
      (let ((project-ids
             (org-entry-get-multivalued-property nil "ORG_GTD_PROJECT_IDS")))
        (if (member (org-get-todo-state) org-done-keywords)
            (progn
              (org2calendar--repair-gtd-projects project-ids)
              (list :status 'skipped :org-id org-id :reason "already-done"))
          (let ((done-keyword (org2calendar--done-keyword))
                (org-log-done 'time))
            (org-todo done-keyword)
            (org2calendar--set-closed-time org-id marker completion-time)
            (org2calendar--repair-gtd-projects project-ids)
            (list :status 'completed
                  :org-id org-id
                  :fallback-time fallback-time)))))))

(defun org2calendar--apply-reminder-completion (record imported-at)
  "Apply completed Reminder RECORD to Org, using IMPORTED-AT as fallback.

Return a plist containing `:status', `:org-id' and diagnostic fields."
  (condition-case error-data
      (let* ((org-id (alist-get :org-id record))
             (completed (alist-get :completed record))
             (completed-at (alist-get :completed-at record))
             (fallback-time (null completed-at))
             (completion-time
              (org2calendar--completion-time completed-at imported-at))
             (marker (and (stringp org-id)
                          (not (string-empty-p org-id))
                          (org-id-find org-id t))))
        (cond
         ((not completed)
          (list :status 'failed :org-id org-id :reason "not-completed"))
         ((not (org2calendar--valid-marker-p marker))
          (list :status 'failed :org-id org-id :reason "org-id-not-found"))
         (t
          (org2calendar--complete-marker
           marker org-id completion-time fallback-time))))
    (error
     (list :status 'failed
           :org-id (alist-get :org-id record)
           :reason "elisp-error"
           :error (error-message-string error-data)))))

(defun org2calendar--record-reminder-pull-result (summary result)
  "Add completion RESULT to reminder pull SUMMARY."
  (pcase (plist-get result :status)
    ('completed
     (cl-incf (org2calendar-reminder-pull-summary-completed summary))
     (when (plist-get result :fallback-time)
       (cl-incf (org2calendar-reminder-pull-summary-fallback-time summary))))
    ('skipped
     (cl-incf (org2calendar-reminder-pull-summary-skipped summary)))
    (_
     (cl-incf (org2calendar-reminder-pull-summary-failed summary))))
  (push result (org2calendar-reminder-pull-summary-details summary)))

(defun org2calendar--reminder-pull-message (summary list-name)
  "Display Reminder pull SUMMARY for LIST-NAME."
  (message
   "[org2calendar ← %s] 完成:%d 跳过:%d 失败:%d（完成时间兜底:%d）"
   list-name
   (org2calendar-reminder-pull-summary-completed summary)
   (org2calendar-reminder-pull-summary-skipped summary)
   (org2calendar-reminder-pull-summary-failed summary)
   (org2calendar-reminder-pull-summary-fallback-time summary)))

(defun org2calendar--auto-pull-delay ()
  "Return seconds before the next automatic pull may run."
  (if (null org2calendar--auto-pull-last-finished-at)
      0
    (max 0
         (- org2calendar-auto-pull-debounce-seconds
            (- (float-time) org2calendar--auto-pull-last-finished-at)))))

(defun org2calendar--run-auto-pull ()
  "Run one automatic Reminder pull when the mode is active."
  (setq org2calendar--auto-pull-pending-timer nil)
  (when (and org2calendar-auto-pull-mode
             (not org2calendar--auto-pull-running))
    (setq org2calendar--auto-pull-running t)
    (unwind-protect
        (condition-case error-data
            (org2calendar-pull-reminder-completions)
          (error
           (message "org2calendar 自动拉取失败: %s"
                    (error-message-string error-data))))
      (setq org2calendar--auto-pull-running nil
            org2calendar--auto-pull-last-finished-at (float-time)))))

(defun org2calendar--request-auto-pull (&rest _ignored)
  "Schedule one debounced automatic pull without blocking the caller."
  (when (and org2calendar-auto-pull-mode
             (not org2calendar--auto-pull-running)
             (null org2calendar--auto-pull-pending-timer))
    (setq org2calendar--auto-pull-pending-timer
          (run-with-idle-timer
           (org2calendar--auto-pull-delay) nil
           #'org2calendar--run-auto-pull))))

(defun org2calendar--cancel-auto-pull-timer (variable)
  "Cancel timer stored in VARIABLE and clear it."
  (when-let* ((timer (symbol-value variable)))
    (cancel-timer timer)
    (set variable nil)))

(defun org2calendar--enable-auto-pull ()
  "Install automatic Reminder pull triggers."
  (add-hook 'emacs-startup-hook #'org2calendar--request-auto-pull)
  (add-hook 'focus-in-hook #'org2calendar--request-auto-pull)
  (org2calendar--cancel-auto-pull-timer 'org2calendar--auto-pull-idle-timer)
  (setq org2calendar--auto-pull-idle-timer
        (run-with-idle-timer
         org2calendar-auto-pull-idle-seconds t
         #'org2calendar--request-auto-pull))
  (org2calendar--request-auto-pull))

(defun org2calendar--disable-auto-pull ()
  "Remove automatic Reminder pull triggers and timers."
  (remove-hook 'emacs-startup-hook #'org2calendar--request-auto-pull)
  (remove-hook 'focus-in-hook #'org2calendar--request-auto-pull)
  (org2calendar--cancel-auto-pull-timer 'org2calendar--auto-pull-idle-timer)
  (org2calendar--cancel-auto-pull-timer 'org2calendar--auto-pull-pending-timer))

;;;###autoload
(define-minor-mode org2calendar-auto-pull-mode
  "Automatically pull Reminder completions on startup, focus and idle."
  :global t
  :group 'org2calendar
  (if org2calendar-auto-pull-mode
      (org2calendar--enable-auto-pull)
    (org2calendar--disable-auto-pull)))

;;;###autoload
(defun org2calendar-sync-agenda-tasks (&optional list-name)
  "Sync selected agenda tasks to the Apple Reminders LIST-NAME.

Use bulk-marked entries when present, otherwise an active region, otherwise the
task on the current agenda line.  Return an
`org2calendar-sync-summary' for programmatic callers."
  (interactive)
  (unless (fboundp 'org2calendar-sync-reminder)
    (user-error "org2calendar 动态模块尚未加载"))
  (let* ((target-list (or list-name org2calendar-reminder-list))
         (summary (org2calendar--new-sync-summary)))
    (dolist (marker (org2calendar--agenda-markers))
      (org2calendar--sync-marker marker target-list summary))
    (setf (org2calendar-sync-summary-details summary)
          (nreverse (org2calendar-sync-summary-details summary)))
    (org2calendar--summary-message summary target-list)
    summary))

;;;###autoload
(defun org2calendar-pull-reminder-completions (&optional list-name start end)
  "Pull completed reminders from LIST-NAME into their Org tasks.

START and END are optional Emacs time values.  Without them, query the last
`org2calendar-reminder-lookback-days' through the current time.  Return an
`org2calendar-reminder-pull-summary'."
  (interactive)
  (unless (fboundp 'org2calendar-fetch-completed-reminders)
    (user-error "org2calendar 动态模块尚未加载或版本过旧"))
  (let* ((target-list (or list-name org2calendar-reminder-list))
         (window (org2calendar--reminder-window start end))
         (records (org2calendar-fetch-completed-reminders
                   target-list (car window) (cadr window)))
         (summary (org2calendar--new-reminder-pull-summary))
         (imported-at (current-time)))
    (if (stringp records)
        (org2calendar--record-reminder-pull-result
         summary (list :status 'failed :reason records))
      (dolist (record records)
        (org2calendar--record-reminder-pull-result
         summary (org2calendar--apply-reminder-completion record imported-at))))
    (setf (org2calendar-reminder-pull-summary-details summary)
          (nreverse (org2calendar-reminder-pull-summary-details summary)))
    (org2calendar--reminder-pull-message summary target-list)
    summary))

(provide 'org2calendar)

;;; org2calendar.el ends here
