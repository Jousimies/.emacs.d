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
	 ("C-<f12>" . org-gtd-process-inbox)
	 :map org-gtd-clarify-map
	 ("C-c C-c" . org-gtd-organize))
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


(add-hook 'org-agenda-mode-hook (lambda ()
				  (unless (featurep 'org-gtd-projects)
				    (require 'org-gtd-projects))))

(with-eval-after-load 'org
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
 (defun my/org-clock-out-to-ms-calendar ()
  "Clock-out 时提取标题与起止时间，异步传给 C# 程序写入微软日历。"
  (let* ((title (org2calendar-get-context-summary))
         (start-time org-clock-start-time)
         (end-time (current-time))
         (start-str (format-time-string "%Y-%m-%d %H:%M:%S" start-time))
         (end-str (format-time-string "%Y-%m-%d %H:%M:%S" end-time))
         (exe-path (expand-file-name "module/EmacsCalendarSync.exe" user-emacs-directory)))

    (if (and title start-time end-time)
        (progn
          ;; 日志会输出到 *Org-Cal-Sync* buffer 中
          (start-process "org-ms-cal-sync"
                         "*Org-Cal-Sync*"
                         exe-path
                         title
                         start-str
                         end-str)
          (message " [日历同步] 已发送: %s (%s -> %s)" title start-str end-str))
      (message "[日历同步] 错误: 未能抓取到有效的 Clock 节点信息")))))

(when (eq system-type 'windows-nt)
  (add-hook 'org-clock-out-hook #'my/org-clock-out-to-ms-calendar))

(provide 'init-gtd)
