;; -*- lexical-binding: t; -*-
(defvar icloud "~/Library/Mobile Documents/"
  "This folder contains documents in icloud.")

(defvar cache-directory (expand-file-name ".cache" user-emacs-directory))

(setopt org-agenda-window-setup 'only-window
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-todo-ignore-scheduled 'future
        org-agenda-todo-ignore-deadlines 'near
        org-agenda-dim-blocked-tasks t
        org-agenda-compact-blocks t
        org-agenda-align-tags-to-column 120
        org-deadline-warning-days 7)

(with-eval-after-load 'org-clock
  (org-clock-persistence-insinuate)
  (setopt org-clock-persist-file (expand-file-name "org-clock-save.el" cache-directory)
	  org-clock-history-length 23
	  org-clock-in-resume t
	  org-clock-into-drawer "LOGCLOCK"
	  org-clock-idle-time 15
	  org-clock-out-remove-zero-time-clocks t
	  org-clock-out-when-done t
	  org-clock-persist 'history
	  org-clock-clocktable-default-properties '(:maxlevel 5 :link t :tags t)
	  org-clock-persist-query-resume nil
	  org-clock-report-include-clocking-task t
	  org-clock-sound "/System/Library/Sounds/Ping.aiff")

  (add-hook 'org-after-todo-state-change-hook (lambda ()
                                                (if (org-clocking-p)
                                                    (org-clock-out)))))


(use-package org-gtd
  :load-path "~/.emacs.d/packages/org-gtd.el/" "~/.emacs.d/packages/org-edna/" "~/.emacs.d/packages/dag-draw.el/" "~/.emacs.d/packages/ht.el/" "~/.emacs.d/packages/f.el/" "~/.emacs.d/packages/s.el/" "~/.emacs.d/packages/dash.el/"

  :init
  (setq org-gtd-update-ack "4.0.0")
  (setopt org-gtd-directory (expand-file-name "iCloud~com~appsonthemove~beorg/Documents/org" icloud))
  :custom
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
  (org-edna-mode 1))

(unless (featurep 'org-gtd-wip)
  (require 'org-gtd-wip))

(with-eval-after-load 'org
  (setopt org-agenda-files (list org-gtd-directory)))

(with-eval-after-load 'org-agenda
  (unless (featurep 'org-gtd-agenda-transient)
    (require 'org-gtd-agenda-transient)))

(with-eval-after-load 'org-gtd-clarify
  (unless (featurep 'org-gtd-organize)
    (require 'org-gtd-organize))
  (define-key org-gtd-clarify-mode-map (kbd "C-c C-c") #'org-gtd-organize))

(defun my/org-gtd-Reading ()
  "Show all projects, including completed ones."
  (interactive)
  (org-gtd-view-show
   '(((name . "Today's Schedule")
      (block-type . calendar-day))
     ((name . "Reading Tasks")
      (area-of-focus . "Reading")
      (type . next-action)))))

(defun my/org-gtd-work ()
  (interactive)
  (org-gtd-view-show
   '(((name . "Actions with A Priority")
      (type . next-action)
      (priority . A))
     ((name . "Today's Schedule")
      (block-type . calendar-day))
     ((name . "Active Projects")
      (type . active-project))
     ((name . "Actions Related to Work")
      (type . next-action)
      (area-of-focus . "Work")))))

(defun my/org-gtd-engage-view-spec ()
  `((name . "GTD Engage View")
    (prefix . (project area-of-focus "—"))
    (prefix-width . ,org-gtd-prefix-width)
    (blocks . (((name . "Today's Schedule")
		(block-type . calendar-day))
	       ((name . "Actions with A Priority")
		(type . next-action)
		(priority . A))	       	       
	       ((name . "Delegated tasks")
		(type . delegated)
		)
	       ((name . "All actions ready to be executed")
		(type . next-action)		
		)))))

(advice-add 'org-gtd-engage-view-spec :override #'my/org-gtd-engage-view-spec)


(add-hook 'after-init-hook 'org-gtd-mode)

(defun my/org-gtd-exit-and-close ()
  "保存所有 Org 缓冲区并彻底退出 Emacs。"
  (interactive)
  (org-save-all-org-buffers)
  (kill-emacs))

;; 将退出函数绑定到 Agenda 视图的 q 键上
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "q") 'my/org-gtd-exit-and-close))
