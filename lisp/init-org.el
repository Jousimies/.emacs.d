;;; init-org.el ---  Org.	-*- lexical-binding: t no-byte-compile: t -*-
;;; Code:
;;; Commentary:
;; Better default.
(with-eval-after-load 'org
  (setq org-modules '()
        org-imenu-depth 4
        org-return-follows-link t
        org-image-actual-width nil
        org-display-remote-inline-images 'download
        org-log-into-drawer t
        org-fast-tag-selection-single-key 'expert
        org-adapt-indentation nil
        org-fontify-quote-and-verse-blocks t
        org-support-shift-select t
        org-treat-S-cursor-todo-selection-as-state-change nil
        org-hide-leading-stars nil
        org-startup-with-inline-images t
        org-image-actual-width '(500)
        org-use-speed-commands t))

;; Tasks states.
(with-eval-after-load 'org
  (setq org-todo-repeat-to-state t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "INPROGRESS(i)" "|" "WAIT(w@)" "SOMEDAY(s@)" "CNCL(c@/!)" "DONE(d)")))
  (setq org-todo-state-tags-triggers
        (quote (("CNCL" ("CNCL" . t))
                ("WAIT" ("WAIT" . t))
                ("SOMEDAY" ("WAIT") ("SOMEDAY" . t))
                (done ("WAIT") ("SOMEDAY"))
                ("TODO" ("WAIT") ("CNCL") ("SOMEDAY"))
                ("NEXT" ("WAIT") ("CNCL") ("SOMEDAY"))
                ("DONE" ("WAIT") ("CNCL") ("SOMEDAY"))))))

;; Babel
;; https://emacs-china.org/t/org-babel/18699
(with-eval-after-load 'org
  (setq org-babel-python-command "python3")
  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((emacs-lisp . t)))
  (defun my/org-babel-execute-src-block (&optional _arg info _params)
    "Load language if needed"
    (let* ((lang (nth 0 info))
           (sym (if (member (downcase lang) '("c" "cpp" "c++")) 'C (intern lang)))
           (backup-languages org-babel-load-languages))
      ;; - (LANG . nil) 明确禁止的语言，不加载。
      ;; - (LANG . t) 已加载过的语言，不重复载。
      (unless (assoc sym backup-languages)
        (condition-case err
            (progn
              (org-babel-do-load-languages 'org-babel-load-languages (list (cons sym t)))
              (setq-default org-babel-load-languages (append (list (cons sym t)) backup-languages)))
          (file-missing
           (setq-default org-babel-load-languages backup-languages)
           err)))))
  (advice-add 'org-babel-execute-src-block :before 'my/org-babel-execute-src-block)
  (setq org-confirm-babel-evaluate nil))

;; Capture
(with-eval-after-load 'org
  (setq org-capture-templates '(("i" "Inbox"
                                 plain (file+olp+datetree (lambda () (concat my-galaxy "/inbox/inbox.org")))
                                 "**** %?\n%U\n" :time-prompt t :tree-type week)
                                ("a" "Anki Deck")
                                ("ae" "Deck: English"
                                 entry (file (lambda ()
                                               (concat my-galaxy "/anki/anki_english.org")))
                                 "* %?\n" :jump-to-captured t)
                                ("ac" "Deck: Civil Engineering"
                                 entry (file (lambda ()
                                               (concat my-galaxy "/anki/anki_engineering.org")))
                                 "* %?\n" :jump-to-captured t)
                                ("s" "Code snippets"
                                 entry (file (lambda ()
                                               (concat my-galaxy "/scripts/snippets.org")))
                                 "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
                                ("l" "Lists")
                                ("lm" "Movie"
                                 entry (file+headline (lambda () (concat my-galaxy "/roam/main/movie.org")) "Movie list")
                                 "* %?
  :PROPERTIES:
  :GENRE: %^{Film genre|Action|Adventure|Comedy|Drama|Fantasy|Horror|Musicals|Mystery|Romance|Science fiction|Sports|Thriller}
  :COUNTRY:
  :SCORE:
  :PLOT: %^{PLOT}
  :END:")))

  (global-set-key (kbd "<f10>") 'org-capture)
  (general-define-key
   :keymaps 'org-capture-mode-map
   [remap evil-save-and-close]          'org-capture-finalize
   [remap evil-save-modified-and-close] 'org-capture-finalize
   [remap evil-quit]                    'org-capture-kill)

  (general-define-key
   :states 'normal
   :keymaps 'org-capture-mode-map
   "RET" "C-c C-c"
   "SPC k" '(org-capture-kill :which-key "abort capture")))

;; Refile
(with-eval-after-load 'org
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-active-region-within-subtree t))

;; ol
(with-eval-after-load 'ol
  (setq org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                               (vm-imap . vm-visit-imap-folder-other-frame)
                               (gnus . org-gnus-no-new-news)
                               (file . find-file)
                               (wl . wl-other-frame))))

;; Archive
(with-eval-after-load 'org
  (setq org-archive-location (expand-file-name "todos/gtd_archive.org::datetree/" my-galaxy)))
(defun my/gtd-archive ()
  "Archive tasks to specific file."
  (interactive)
  (find-file (expand-file-name "todos/gtd_archive.org" my-galaxy)))

;; Habit.
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))

;; Src block.
(with-eval-after-load 'org
  ;;(setq org-src-window-setup 'current-window)
  (setq org-src-ask-before-returning-to-edit-buffer nil))

;; Org ID.
(with-eval-after-load 'org
  (setq org-id-method 'ts)
  (setq org-id-link-to-org-use-id 'create-if-interactive))

(defun my/copy-idlink ()
  "Copy idlink to clipboard."
  (interactive)
  (when (eq major-mode 'org-agenda-mode) ;switch to orgmode
    (org-agenda-show)
    (org-agenda-goto))
  (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
    (let* ((mytmphead (nth 4 (org-heading-components)))
           (mytmpid (funcall 'org-id-get-create))
           (mytmplink (format "- [ ] [[id:%s][%s]]" mytmpid mytmphead)))
      (kill-new mytmplink)
      (message "Copied %s to killring (clipboard)" mytmplink))))
(global-set-key (kbd "<f7>") 'my/copy-idlink)

;; Clock.
(with-eval-after-load 'org
  ;;(org-clock-persistence-insinuate)
  (setq org-clock-history-length 23)
  (setq org-clock-in-resume t)
  (setq org-clock-into-drawer "LOGCLOCK")
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-out-when-done t)
  (setq org-clock-persist t)
  (setq org-clock-clocktable-default-properties '(:maxlevel 5 :link t :tags t))
  (setq org-clock-persist-query-resume nil)
  (setq org-clock-report-include-clocking-task t)
  ;; (setq org-clock-out-switch-to-state "DONE")
  (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
  (setq bh/keep-clock-running nil))

(general-define-key
 :states '(normal visual emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "oc" '(:ignore t :wk "Clock")
 "ocj" '(org-clock-goto :wk "Clock goto")
 "oci" '(org-clock-in :wk "Clock In")
 "oco" '(org-clock-out :wk "Clock Out"))

;; Punch In and Punch Out.
(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))
(defun bh/find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))
(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))
(defun bh/clock-in-to-next ()
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))
(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the selected task.
If `ARG' is nil, set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-default-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-default-task-as-default)))))
(defun bh/punch-out ()
  "Punch out."
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))
(defun bh/clock-in-default-task ()
  "Clock In default task with specific ID."
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))
(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in."
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))
(defvar bh/default-task-id "20220524T114723.420565")
(defun bh/clock-in-default-task-as-default ()
  "Clock in default task."
  (interactive)
  (org-with-point-at (org-id-find bh/default-task-id 'marker)
    (org-clock-in '(16))))
(defun bh/clock-out-maybe ()
  "Clock out."
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one.
Skip the default task and get the next one.
A prefix `ARG' forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(general-define-key
 :keymaps '(normal visual emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "ti" '(bh/punch-in :wk "Punch In")
 "to" '(bh/punch-out :wk "Punch Out"))

;; Agenda
(with-eval-after-load 'org
  (setq org-agenda-files (directory-files-recursively (expand-file-name "todos" my-galaxy) "org$\\|archive$"))
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-compact-blocks t)

  (defun my/gtd-file ()
    (interactive)
    (find-file (expand-file-name "todos/gtd.org" my-galaxy)))

  (add-hook 'org-agenda-finalize-hook #'org-agenda-find-same-or-today-or-agenda 90)

  (define-key org-mode-map (kbd "C-,") nil)
  (define-key org-mode-map (kbd "C-'") nil)

  (setq org-agenda-custom-commands
        '(("A" "Archive"
           ((todo "DONE|CNCL"
                  ((org-agenda-prefix-format " %i")
                   (org-agenda-hide-tags-regexp "project")
                   (org-agenda-overriding-header "Archive")))))
          (" " "GTD Lists: Daily agenda and tasks"
           ((agenda "" ((org-agenda-span 2)
                        (org-deadline-warning-days 3)
                        (org-agenda-block-separator nil)
                        (org-scheduled-past-days 365)
                        (org-agenda-hide-tags-regexp "project")
                        (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                        (org-agenda-format-date "%A %-e %B %Y")
                        (org-agenda-prefix-format " %i %?-12t% s")
                        (org-agenda-overriding-header "Today's agenda")))
            (tags-todo "*"
                       ((org-agenda-skip-function
                         `(org-agenda-skip-entry-if 'deadline
                                                    'schedule
                                                    'timestamp
                                                    'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                        (org-agenda-hide-tags-regexp "project")
                        (org-agenda-prefix-format " %i")
                        (org-agenda-overriding-header "Important tasks without a date")))
            (todo "NEXT"
                  ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                   (org-agenda-prefix-format " %i")
                   (org-agenda-block-separator nil)
                   (org-agenda-hide-tags-regexp "project")
                   (org-agenda-overriding-header "Next tasks list")))
            (todo "INPROGRESS"
                  ((org-agenda-block-separator nil)
                   (org-agenda-prefix-format " %i")
                   (org-agenda-hide-tags-regexp "project")
                   (org-agenda-overriding-header "Inprogress tasks list")))
            (tags-todo "-Computer-Emacs/TODO"
                       ((org-agenda-skip-function
                         `(org-agenda-skip-entry-if 'deadline
                                                    'schedule
                                                    'timestamp
                                                    'regexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                        (org-agenda-prefix-format " %i")
                        (org-agenda-hide-tags-regexp "project")
                        (org-agenda-block-separator nil)
                        (org-agenda-overriding-header "Todo tasks list")))
            (tags-todo "Emacs|Computer"
                       ((org-agenda-block-separator nil)
                        (org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                        (org-agenda-prefix-format " %i")
                        (org-agenda-hide-tags-regexp "project")
                        (org-agenda-overriding-header "Computer science")))
            (tags-todo "Family"
                       ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                        (org-agenda-prefix-format " %i")
                        (org-agenda-hide-tags-regexp "project")
                        (org-agenda-block-separator nil)
                        (org-agenda-overriding-header "Family")))
            (todo "WAIT|SOMEDAY"
                  ((org-agenda-block-separator nil)
                   (org-agenda-prefix-format " %i")
                   (org-agenda-hide-tags-regexp "project")
                   (org-agenda-overriding-header "Tasks on hold")))))))

  (defun my/org-agenda ()
    "Open my org-agenda."
    (interactive)
    (org-agenda "" " "))

  (global-set-key (kbd "<f12>") 'my/org-agenda)

  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "o" '(:ignore t :wk "Org")
   "oa" '(my/org-agenda :wk "Agenda")
   "ot" '(org-todo-list :wk "Todo list")
   "ov" '(org-search-view :wk "View search")))



(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
