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
  :vc (:url "https://github.com/Trevoke/org-gtd.el.git"
	    :rev "master")
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
  (keymap-set org-gtd-clarify-map "C-c C-c" #'org-gtd-organize))

(add-hook 'org-agenda-mode-hook (lambda ()
				  (unless (featurep 'org-gtd-projects)
				    (require 'org-gtd-projects))))

(with-eval-after-load 'org

  (defcustom org2calendar-exe-path
    (expand-file-name "module/EmacsCalendarSync.exe" user-emacs-directory)
    "Path to the external calendar sync executable."
    :type 'string
    :group 'org2calendar)

  (defun org2calendar-get-context-summary ()
    "获取标题摘要。如果父标题存在且不属于特定的过滤词，则返回 '父标题 / 当前标题'。
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

  (defun org2calendar--send-event (title start-time end-time)
    "Send TITLE with START-TIME and END-TIME to external sync program via UTF-8 Stdin."
    (unless (and title start-time end-time)
      (message "[日历同步] 错误: 标题或时间缺失")
      (cl-return-from org2calendar--send-event nil))

    (let* ((start-str (if (stringp start-time)
                          start-time
                        (format-time-string "%Y-%m-%d %H:%M:%S" start-time)))
           (end-str   (if (stringp end-time)
                          end-time
                        (format-time-string "%Y-%m-%d %H:%M:%S" end-time)))
           ;; 1. 强制设定当前通信的编码为 UTF-8
           (coding-system-for-write 'utf-8)
           (coding-system-for-read 'utf-8)
           ;; 2. 启动进程时不附带参数，避免被 Windows 命令行 ANSI 转码污染
           (proc (start-process "org-ms-cal-sync"
                                "*Org-Cal-Sync*"
                                org2calendar-exe-path))
           ;; 3. 按行拼接 payload 内容
           (payload (concat title "\n" start-str "\n" end-str "\n")))

      ;; 4. 将 payload 写入 Stdin 并关闭写通道 (EOF)
      (process-send-string proc payload)
      (process-send-eof proc)

      (message " [日历同步] 已发送: %s (%s -> %s)" title start-str end-str)))

  (defun org2calendar-send-to-ms ()
    (interactive)
    (let ((title (org2calendar-get-context-summary))
          (start org-clock-start-time)
          (end   (current-time)))
      (org2calendar--send-event title start end)))

  (defun org2calendar-send-to-ms-at-point ()
    (interactive)
    (save-excursion
      (org-back-to-heading t)
      (let ((end-of-subtree (save-excursion (org-end-of-subtree) (point))))
        (if (re-search-forward "CLOCK: \\(\\[.*?\\]\\)--\\(\\[.*?\\]\\)" end-of-subtree t)
            (let* ((start-ts (match-string 1))
                   (end-ts   (match-string 2))
                   (start    (org-time-string-to-time start-ts))
                   (end      (org-time-string-to-time end-ts))
                   (title    (org2calendar-get-context-summary)))
              (org2calendar--send-event title start end))
          (message "未在当前条目下找到 CLOCK 记录，无法同步。")))))

  (keymap-global-set "C-c C-x p" #'org2calendar-send-to-ms-at-point))

(when (eq system-type 'windows-nt)
  (add-hook 'org-clock-out-hook #'org2calendar-send-to-ms))

(provide 'init-gtd)
