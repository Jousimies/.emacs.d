;; -*- lexical-binding: t; -*-

(defvar icloud "~/Library/Mobile Documents/"
  "This folder contains documents in icloud.")

(setopt org-gtd-directory (expand-file-name "iCloud~com~appsonthemove~beorg/Documents/org" icloud))
(setopt org-agenda-files (list org-gtd-directory))

(defun my/org-batch-generate-report-recent ()
  (interactive)
  (require 'org-datetree)
  (dolist (day-offset (number-sequence 7 0 -1))
    (let* ((target-time (time-subtract (current-time) (days-to-time day-offset)))
           (year (format-time-string "%Y" target-time))
           (month (string-to-number (format-time-string "%m" target-time)))
           (day (string-to-number (format-time-string "%d" target-time)))
           (today-date (list month day (string-to-number year)))
           (today-str (format-time-string "%Y-%m-%d %A" target-time))
           (report-file (expand-file-name (format "org_report_%s.org" year) org-gtd-directory)))

      (with-current-buffer (find-file-noselect report-file)
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward (concat "^\\*\\*\\* " (regexp-quote today-str)) nil t)
              (let ((end (save-excursion (outline-next-heading) (point))))
                (if (re-search-forward "#\\+BEGIN: clocktable" end t)
                    (message "跳过：%s 报告已存在。" today-str)
                  (progn
                    (my/org-insert-and-update-clocktable target-time)
                    (message "更新：%s 报告已补充 clocktable。" today-str))))
            (progn
              (org-datetree-find-date-create today-date)
              (my/org-insert-and-update-clocktable target-time)
              (message "新建：%s 报告已生成。" today-str))))
        (save-buffer)))))

(defun my/org-insert-and-update-clocktable (target-time)
  "在当前位置插入指定日期的 clocktable 并更新。"
  (let ((date-str (format-time-string "%Y-%m-%d" target-time)))
    (end-of-line)
    (insert "\n#+BEGIN: clocktable :scope org-agenda-files :maxlevel 9 :block " 
            date-str 
            " :fileskip0 t \n#+END:\n")
    (forward-line -2)
    (org-update-dblock)))
