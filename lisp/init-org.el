;; init-org.el --- org-mode *- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package org
  :config
  (setq org-ellipsis " ⇲")
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
        org-use-speed-commands t)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)

  (setq org-todo-repeat-to-state t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "INPROGRESS(i)" "|" "WAIT(w@)" "SOMEDAY(s@)" "CNCL(c@/!)" "DONE(d)")))

  (setq org-todo-keyword-faces
        '(("NEXT" . '(success org-todo))
          ("TODO" . org-todo)
          ("CNCL" . '(region org-todo))
          ("WAIT" . '(bold org-todo))))

  (setq org-priority-faces
        '((?A . '(bold org-priority))
          (?B . org-priority)
          (?C . '(shadow org-priority))))
  (setq org-todo-state-tags-triggers
        (quote (("CNCL" ("CNCL" . t))
                ("WAIT" ("WAIT" . t))
                ("SOMEDAY" ("WAIT") ("SOMEDAY" . t))
                (done ("WAIT") ("SOMEDAY"))
                ("TODO" ("WAIT") ("CNCL") ("SOMEDAY"))
                ("NEXT" ("WAIT") ("CNCL") ("SOMEDAY"))
                ("DONE" ("WAIT") ("CNCL") ("SOMEDAY")))))
  :bind (:map org-mode-map
              ("C-c l" . org-store-link)))
;; ("<return>" . org-return)))

(use-package ob-core
  :after org
  :config
  (defun my/org-babel-execute-src-block (&optional _arg info _params)
    "Load language if needed"
    (let* ((lang (nth 0 info))
           (sym (if (member (downcase lang) '("c" "cpp" "c++")) 'C (intern lang)))
           (backup-languages org-babel-load-languages))
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

(use-package org-capture
  :after org
  :bind (:map org-capture-mode-map
              ([remap evil-save-and-close] . org-capture-finalize)
              ([remap evil-save-modified-and-close] . org-capture-finalize)
              ([remap evil-quit] . org-capture-kill)
              ("RET" . org-capture-finalize))
  :config
  (setq org-capture-templates
        '(("i" "GTD Inbox"
           entry (file (lambda () (concat my-galaxy "/todos/inbox.org")))
           "* %?\n%U\n" :time-prompt t :tree-type week)
          ("I" "NC Inbox"
           plain (file+olp+datetree (lambda () (concat my-galaxy "/inbox/inbox.org")))
           "**** %?\n%U\n" :time-prompt t :tree-type week)
          ("p" "Daily Plan"
           plain (file+olp+datetree (lambda () (concat my-galaxy "/inbox/plan.org")))
           "- [ ] %?\n%U\n" :time-prompt t :tree-type week)
          ("r" "Reflection"
           plain
           (file+olp+datetree (lambda () (concat my-galaxy "/roam/main/reflection.org")))
           (file "~/.emacs.d/template/tpl-daily-reflection")
           :time-prompt t :tree-type week)
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
          ;; ("l" "Lists")
          ("m" "Movie"
           entry (file+headline (lambda () (concat my-galaxy "/roam/main/watchlist.org")) "Watching Lists")
           "* %?
:PROPERTIES:
:GENRE: %^{Film genre|Action|Adventure|Comedy|Drama|Fantasy|Horror|Musicals|Mystery|Romance|Science fiction|Sports|Thriller}
:COUNTRY:
:SCORE:
:PLOT: %^{PLOT}
:END:"))))

(defun my/org-capture ()
  "Make a new frame to do org-capture staff."
  (interactive)
  (make-frame)
  (org-capture))

(add-hook 'org-capture-after-finalize-hook 'delete-frame)

;; (add-to-list 'display-buffer-alist '("\\*Org Select\\*"
;;                                      (display-buffer-pop-up-frame)
;;                                      (window-parameters
;;                                       (no-other-window . t)
;;                                       (mode-line-format . none)
;;                                       (no-delete-other-windows . t))))

(global-set-key (kbd "<f10>") 'my/org-capture)

(use-package org-attach
  :config
  (setq org-attach-id-dir (expand-file-name "attach" my-galaxy))
  (defun my/org-attach-visit-headline-from-dired ()
    "Go to the headline corresponding to this org-attach directory."
    (interactive)
    (let* ((id-parts (last (split-string default-directory "/" t) 2))
           (id (apply #'concat id-parts)))
      (let ((m (org-id-find id 'marker)))
        (unless m (user-error "Cannot find entry with ID \"%s\"" id))
        (pop-to-buffer (marker-buffer m))
        (goto-char m)
        (move-marker m nil)
        (org-fold-show-context))))
  (add-to-list 'display-buffer-alist
               '("\\*Org Attach\\*"
                 (display-buffer-pop-up-frame)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.5)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  :bind (:map dired-mode-map
              ("C-'" . my/org-attach-visit-headline-from-dired)))

(use-package org-habit
  :after org-agenda
  :config
  (add-to-list 'org-modules 'org-habit t))

(use-package org-id
  :after org
  :config
  (setq org-id-locations-file (expand-file-name "cache/.org-id-locations" user-emacs-directory))
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

(my/space-leader-def
  "oi" '(org-id-get-create :wk "Create ID"))

(use-package org-src
  :after org
  :config
  (setq org-src-window-setup 'current-window)
  (setq org-src-ask-before-returning-to-edit-buffer nil))

(use-package org-refile
  :after org
  :config
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-active-region-within-subtree t))

(use-package org
  :config
  (defface my-org-emphasis-bold
    '((default :inherit bold)
      (((class color) (min-colors 88) (background light))
       :foreground "#a60000")
      (((class color) (min-colors 88) (background dark))
       :foreground "#ff8059"))
    "My bold emphasis for Org.")

  (defface my-org-emphasis-italic
    '((default :inherit italic)
      (((class color) (min-colors 88) (background light))
       :foreground "#005e00")
      (((class color) (min-colors 88) (background dark))
       :foreground "#44bc44"))
    "My italic emphasis for Org.")

  (defface my-org-emphasis-underline
    '((default :inherit underline)
      (((class color) (min-colors 88) (background light))
       :foreground "#813e00")
      (((class color) (min-colors 88) (background dark))
       :foreground "#d0bc00"))
    "My underline emphasis for Org.")

  (defface my-org-emphasis-strike-through
    '((default :strike-through t)
      (((class color) (min-colors 88) (background light))
       :foreground "#505050")
      (((class color) (min-colors 88) (background dark))
       :foreground "#a8a8a8"))
    "My strike-through emphasis for Org.")

  (defface my-org-emphasis-strike-through
    '((((class color) (min-colors 88) (background light))
       :strike-through "#972500" :foreground "#505050")
      (((class color) (min-colors 88) (background dark))
       :strike-through "#ef8b50" :foreground "#a8a8a8"))
    "My strike-through emphasis for Org.")

  (setq org-emphasis-alist
        '(("*" my-org-emphasis-bold)
          ("/" my-org-emphasis-italic)
          ("_" my-org-emphasis-underline)
          ("=" org-verbatim verbatim)
          ("~" org-code verbatim)
          ("+" my-org-emphasis-strike-through))))

(use-package org-clock
  :after org
  :config
  (org-clock-persistence-insinuate)
  (setq org-clock-persist-file (expand-file-name "cache/org-clock-save.el" user-emacs-directory))
  (setq org-clock-history-length 23)
  (setq org-clock-in-resume t)
  (setq org-clock-into-drawer "LOGCLOCK")
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-out-when-done t)
  (setq org-clock-persist t)
  (setq org-clock-clocktable-default-properties '(:maxlevel 5 :link t :tags t))
  (setq org-clock-persist-query-resume nil)
  (setq org-clock-report-include-clocking-task t))

(my/space-leader-def
  "oc" '(:ignore t :wk "Clock")
  "ocj" '(org-clock-goto :wk "Clock goto")
  "oci" '(org-clock-in :wk "Clock In")
  "oco" '(org-clock-out :wk "Clock Out")
  "ocl" '(org-clock-in-last :wk "Clock In Last"))

(setq bh/keep-clock-running nil)

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
(defun bh/clock-in-to-next (kw) ;; kw should not been deleted.
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
(defvar bh/default-task-id "20230307T170734.341755")
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

(defun my/toggle-punch-in-or-out ()
  "Start clock or stop it when there is a clocking."
  (interactive)
  (if (org-clocking-p)
      (progn
        (bh/punch-out)
        (alert "Wish you have a good day!" :title "Punch Out"))
    (progn
      (bh/punch-in nil)
      (alert "Start Working: Fighting" :title "Punch In" ))))

(my/space-leader-def
  "op" '(my/toggle-punch-in-or-out :wk "Punch In or Out"))

(add-to-list 'display-buffer-alist
             '("\\*Org Note\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.5)
               (window-parameters . ((no-other-window . t)
                                     (no-delete-other-windows . t)))))

;; https://200ok.ch/posts/2022-12-07_streamline_your_org_mode_workflow_with_automatic_clock_table_recalculation.html
;; Need add #+AUTOCALC_CLOCK_TABLES to org file.
(with-eval-after-load 'org
  (add-to-list 'org-options-keywords "AUTOCALC_CLOCK_TABLES:"))

(defun autocalc-clocktable ()
  "Auto update clock table."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char 0)
      (if (string-equal (car
                         (cdr
                          (car
                           (org-collect-keywords '("AUTOCALC_CLOCK_TABLES")))))
                        "t")
          (progn
            (goto-char (search-forward "clocktable"))
            (org-ctrl-c-ctrl-c))))))

(add-hook 'before-save-hook 'autocalc-clocktable)

(defun my/org-find-time-file-property (property &optional anywhere)
  "Return the position of the time file PROPERTY if it exists.
When ANYWHERE is non-nil, search beyond the preamble."
  (save-excursion
    (goto-char (point-min))
    (let ((first-heading
           (save-excursion
             (re-search-forward org-outline-regexp-bol nil t))))
      (when (re-search-forward (format "^#\\+%s:" property)
                               (if anywhere nil first-heading)
                               t)
        (point)))))

(defun my/org-set-time-file-property (property &optional anywhere pos)
  "Set the time file PROPERTY in the preamble.
When ANYWHERE is non-nil, search beyond the preamble.
If the position of the file PROPERTY has already been computed,
it can be passed in POS.

https://github.com/zaeph/.emacs.d/blob/615ac37be6bd78c37e967fdb43d28897a4116583/lisp/zp-org.el#L194"
  (when-let ((pos (or pos
                      (my/org-find-time-file-property property))))
    (save-excursion
      (goto-char pos)
      (if (looking-at-p " ")
          (forward-char)
        (insert " "))
      (delete-region (point) (line-end-position))
      (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert now)))))

(defun my/org-set-date ()
  "Update the LAST_MODIFIED file property in the preamble.
https://github.com/zaeph/.emacs.d/blob/615ac37be6bd78c37e967fdb43d28897a4116583/lisp/zp-org.el#L212"
  (when (and (derived-mode-p 'org-mode)
             (buffer-modified-p))
    (my/org-set-time-file-property "DATE")))

(add-hook 'before-save-hook 'my/org-set-date)

;; Get reading list from books directory for org-clock report.
;; The org-clock report scope can be a function.
(defun my/reading-list ()
  "Get reading list."
  (let (reading-list)
    (append reading-list
            (file-expand-wildcards (expand-file-name "denote/books/*.org" my-galaxy)))))

(provide 'init-org)
;;; init-org.el ends here.
