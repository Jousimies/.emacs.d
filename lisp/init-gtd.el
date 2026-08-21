;; -*- lexical-binding: t; -*-

(with-eval-after-load 'org-agenda
  (setq org-agenda-window-setup 'current-window
	org-agenda-skip-scheduled-if-done t
	org-agenda-skip-deadline-if-done t
	org-agenda-todo-ignore-scheduled 'future
	org-agenda-todo-ignore-deadlines 'near
	org-agenda-dim-blocked-tasks t
	org-agenda-compact-blocks t
	org-agenda-align-tags-to-column 120
	org-deadline-warning-days 7))

(use-package org-gtd
  :ensure (org-gtd :repo "https://github.com/Trevoke/org-gtd.el.git" :branch "master")
  :bind (("<f10>" . org-gtd-capture)
	 ("<f12>" . org-gtd-engage)
	 ("C-<f12>" . org-gtd-process-inbox))
  :init
  (setq org-gtd-update-ack "4.0.0")
  (setq org-gtd-directory (expand-file-name "gtd" my-galaxy))
  :custom
  (org-agenda-files (list org-gtd-directory))
  (org-gtd-refile-to-any-target nil)
  (org-gtd-refile-prompt-for-types '(single-action project-heading calendar someday delegated tickler habit))
  (org-gtd-mode-lighter-display 'when-non-zero)
  (org-use-fast-todo-selection 'expert)
  (org-gtd-clarify-show-horizons 'right)
  (org-gtd-clarify-display-helper-buffer t)
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)" "CNCL(c)")))
  (org-todo-state-tags-triggers
   (quote (("CNCL" ("CNCL" . t))
           ("WAIT" ("WAIT" . t))
           (done ("WAIT"))
           ("TODO" ("WAIT") ("CNCL"))
           ("NEXT" ("WAIT") ("CNCL"))
           ("DONE" ("WAIT") ("CNCL")))))
  (org-gtd-keyword-mapping '((todo . "TODO")
                             (next . "NEXT")
                             (wait . "WAIT")
			     (done . "DONE")
                             (canceled . "CNCL")))
  (org-gtd-areas-of-focus '("Work" "Professional" "Health" "Growth" "Finances" "Leisure" "Home" "Family" "Social"))
  :config
  (org-edna-mode)
  (org-gtd-mode))

(with-eval-after-load 'org-gtd-clarify
  (unless (featurep 'org-gtd-organize)
    (require 'org-gtd-organize))
  (keymap-set org-gtd-clarify-map "C-c C-c" #'org-gtd-organize))

(add-hook 'org-agenda-mode-hook (lambda ()
				  (unless (featurep 'org-gtd-projects)
				    (require 'org-gtd-projects))))

(with-eval-after-load 'org
  (defgroup org2calendar nil
    "Sync Org-mode entries to Microsoft Calendar."
    :group 'org)

  (defcustom org2calendar-exe-path
    (expand-file-name "module/EmacsCalendarSync.exe" user-emacs-directory)
    "Path to the external calendar sync executable."
    :type 'string
    :group 'org2calendar)

  (defcustom org2calendar-account nil
    "Default Microsoft Account email to sync with.
If nil, the program will use the last active account in local cache."
    :type '(choice (const :tag "Use Last Active/Default Account" nil)
                   (string :tag "Account Email"))
    :group 'org2calendar)

  ;; 在 Windows 上设置环境变量 MS_CALENDAR_ACCOUNT 为邮箱地址。
  (when sys/win32p
    (setq org2calendar-account (getenv "MAIL_ACCOUNT")))

  (defun org2calendar-get-context-summary ()
    "获取标题摘要。如果父标题存在且不属于特定的过滤词，则返回 '父标题@当前标题'。
同时移除标题中的统计 Cookie，如 [3/3] 或 [100%]。"
    (let* ((clean-heading (lambda (text)
                            (if text
				(string-trim (replace-regexp-in-string "\\[\\([0-9/]*\\|[0-9%]*\\)\\][ \t]*" "" text))
                              text)))
           (heading (funcall clean-heading (org-get-heading t t t t)))
           (excluded-parents '("Actions" "Calendar" "Projects"))
           (parent-title (save-excursion
                           (when (org-up-heading-safe)
                             (funcall clean-heading (org-get-heading t t t t))))))
      (if (and parent-title
               (not (member parent-title excluded-parents)))
          (format "%s@%s" parent-title heading)
	heading)))

  (defun org2calendar--send-event (title start-time end-time &optional account)
    "Send TITLE with START-TIME and END-TIME to external sync program via UTF-8 Stdin.
Optional argument ACCOUNT specifies the target Microsoft account email."
    (unless (and title start-time end-time)
      (message "[日历同步] 错误: 标题或时间缺失")
      (cl-return-from org2calendar--send-event nil))

    (let* ((start-str (if (stringp start-time)
                          start-time
			(format-time-string "%Y-%m-%d %H:%M:%S" start-time)))
           (end-str   (if (stringp end-time)
                          end-time
			(format-time-string "%Y-%m-%d %H:%M:%S" end-time)))
           (target-acc (or account org2calendar-account))
           ;; 构建命令行参数
           (args (if (and target-acc (not (string-empty-p target-acc)))
                     (list "-a" target-acc)
                   '()))
           ;; 强制设定通信编码为 UTF-8
           (coding-system-for-write 'utf-8)
           (coding-system-for-read 'utf-8)
           ;; 启动外部进程
           (proc (apply #'start-process
			"org-ms-cal-sync"
			"*Org-Cal-Sync*"
			org2calendar-exe-path
			args))
           (payload (concat title "\n" start-str "\n" end-str "\n")))

      ;; 将 payload 写入 Stdin 并关闭 EOF
      (process-send-string proc payload)
      (process-send-eof proc)

      (message "[日历同步] 已发送%s: %s (%s -> %s)"
               (if target-acc (format " [%s]" target-acc) "")
               title start-str end-str)))

  (defun org2calendar-list-accounts ()
    "查看本地已绑定/缓存的所有微软账号。"
    (interactive)
    (let ((coding-system-for-read 'utf-8))
      (async-shell-command (format "%s -l" (shell-quote-argument org2calendar-exe-path))
                           "*Org-Cal-Accounts*")))

  (defun org2calendar-send-to-ms (&optional arg)
    "同步当前 Clock 到微软日历。
按 C-u 执行可临时指定同步账号。"
    (interactive "P")
    (let ((account (when arg (read-string "输入微软账号 Email: " org2calendar-account)))
          (title (org2calendar-get-context-summary))
          (start org-clock-start-time)
          (end   (current-time)))
      (org2calendar--send-event title start end account)))

  (defun org2calendar-send-to-ms-at-point (&optional arg)
    "同步光标所在位置的 CLOCK 记录到微软日历。
使用 C-u 前缀 (例如 C-u C-c C-x p) 可以手动指定/切换微软账号。"
    (interactive "P")
    (let ((account (when arg (read-string "输入微软账号 Email: " org2calendar-account))))
      (save-excursion
	(org-back-to-heading t)
	(let ((end-of-subtree (save-excursion (org-end-of-subtree) (point))))
          (if (re-search-forward "CLOCK: \\(\\[.*?\\]\\)--\\(\\[.*?\\]\\)" end-of-subtree t)
              (let* ((start-ts (match-string 1))
                     (end-ts   (match-string 2))
                     (start    (org-time-string-to-time start-ts))
                     (end      (org-time-string-to-time end-ts))
                     (title    (org2calendar-get-context-summary)))
		(org2calendar--send-event title start end account))
            (message "[日历同步] 未在当前条目下找到 CLOCK 记录，无法同步。")))))))

(when (eq system-type 'windows-nt)
  (add-hook 'org-clock-out-hook #'org2calendar-send-to-ms))

(provide 'init-gtd)
