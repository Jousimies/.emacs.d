;; init-org.el --- Live in plain life with org-mode. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package org
  :bind (:map org-mode-map
              ("M-s-s" . org-store-link))
  :custom
  (org-ellipsis " ⇲")
  (org-modules '())
  (org-imenu-depth 4)
  (org-return-follows-link t)
  (org-display-remote-inline-images 'download)
  (org-log-into-drawer t)
  (org-fast-tag-selection-single-key 'expert)
  (org-adapt-indentation nil)
  (org-fontify-quote-and-verse-blocks t)
  (org-support-shift-select t)
  (org-treat-S-cursor-todo-selection-as-state-change nil)
  (org-hide-leading-stars nil)
  (org-startup-with-inline-images t)
  (org-startup-folded 'content)
  (org-image-actual-width nil)
  (org-use-speed-commands t)
  (org-highlight-latex-and-related '(latex script))
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-tags-sort-function 'org-string-collate-greaterp)
  :config
  (setq org-deadline-warning-days 7)

  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2))
  (setq org-todo-repeat-to-state t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "INPROGRESS(i)" "|" "WAIT(w@)" "SOMEDAY(s@)" "CNCL(c@/!)" "DONE(d)")))
  (add-hook 'org-after-todo-state-change-hook (lambda ()
                                                (if (org-clocking-p)
                                                    (org-clock-out))))
  (defun my/org-refile-on-todo-done ()
    "Refile a task to a different file when it is marked as DONE."
    (let ((org-refile-keep t))
      (when (string= org-state "DONE")
        (org-refile nil nil (list nil (car (denote-journal-extras--entry-today))) "Copy"))))

  (add-hook 'org-after-todo-state-change-hook 'my/org-refile-on-todo-done)

  (setq org-todo-keyword-faces
        '(("TODO" . (:inherit (bold org-todo)))
          ("NEXT" . (:inherit (success org-todo)))
          ("CNCL" . (:inherit (shadow org-todo)))
          ("DONE" . (:inherit (button org-todo)))
          ("WAIT" . (:inherit (warning org-todo)))))

  (setq org-priority-faces
        '((?A . (bold . org-priority))
          (?B . org-priority)
          (?C . (shadow . org-priority))))

  (setq org-todo-state-tags-triggers
        (quote (("CNCL" ("CNCL" . t))
                ("WAIT" ("WAIT" . t))
                ("SOMEDAY" ("WAIT") ("SOMEDAY" . t))
                (done ("WAIT") ("SOMEDAY"))
                ("TODO" ("WAIT") ("CNCL") ("SOMEDAY"))
                ("NEXT" ("WAIT") ("CNCL") ("SOMEDAY"))
                ("DONE" ("WAIT") ("CNCL") ("SOMEDAY")))))
  ;;   此处的设置来源：https://protesilaos.com/emacs/modus-themes
  ;; 若升级 modus-themes 到 4.0 的版本，可能需要修改。
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

  (defface my-org-verbatim
    '((((class color) (min-colors 88) (background light))
       :background "green")
      (((class color) (min-colors 88) (background dark))
       :foreground "green"))
    "My verbatim for Org.")


  (setq org-emphasis-alist
        '(("*" my-org-emphasis-bold)
          ("/" my-org-emphasis-italic)
          ("_" my-org-emphasis-underline)
          ("=" org-verbatim verbatim)
          ("~" my-org-verbatim verbatim)
          ("+" my-org-emphasis-strike-through))))

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
  :bind (("<f10>" . org-capture)
         (:map org-capture-mode-map
               ([remap evil-save-and-close] . org-capture-finalize)
               ([remap evil-save-modified-and-close] . org-capture-finalize)
               ([remap evil-quit] . org-capture-kill)))
  :config
  (setq org-capture-templates
        '(("i" "GTD Inbox"
           entry (file (lambda () (concat mobile-document "iCloud~com~appsonthemove~beorg/Documents/org/inbox.org")))
           "* %?\n%U\n" :time-prompt t :tree-type week)
          ("l" "Today log" plain
           (file+olp+datetree (lambda () (concat my-galaxy "/logs/log_" (format-time-string "%Y") ".org")))
           "**** %?\n%U\n"
           :time-prompt t :tree-type week)
          ("w" "Watch")
          ("wt" "TV drama"
           entry (file+headline (lambda ()
                                  (concat my-galaxy "/logs/watchlist_"
                                          (format-time-string "%Y") ".org"))
                                "Drama")
           "* %?
    :PROPERTIES:
    :GENRE: %^{类型|剧情|犯罪|}
    :COUNTRY:
    :SCORE:
    :PLOT: %^{PLOT}
    :END:")
          ("ws" "视频"
           entry (file+headline (lambda ()
                                  (concat my-galaxy "/logs/watchlist_"
                                          (format-time-string "%Y") ".org"))
                                "视频")
           "* %?")
          ("wc" "cartoon"
           entry (file+headline (lambda ()
                                  (concat my-galaxy "/logs/watchlist_"
                                          (format-time-string "%Y") ".org"))
                                "Cartoon")
           "* %?")
          ("wm" "Movie"
           entry (file+headline (lambda ()
                                  (concat my-galaxy "/logs/watchlist_"
                                          (format-time-string "%Y") ".org"))
                                "Movie")
           "* %?
:PROPERTIES:
:GENRE: %^{Film genre|Action|Adventure|Comedy|Drama|Fantasy|Horror|Musicals|Mystery|Romance|Science fiction|Sports|Thriller}
:COUNTRY:
:SCORE:
:PLOT: %^{PLOT}
:END:"))))

(use-package org-attach
  :hook (org-attach-after-change . org-attach-save-file-list-to-property)
  :config
  (setq org-attach-expert t)
  (setq org-attach-id-dir (expand-file-name "attach" my-galaxy))
  (setq org-attach-id-to-path-function-list
        '(org-attach-id-ts-folder-format
          org-attach-id-uuid-folder-format))

  (defun org-attach-save-file-list-to-property (dir)
    "Save list of attachments to ORG_ATTACH_FILES property."
    (when-let* ((files (org-attach-file-list dir)))
      (org-set-property "ORG_ATTACH_FILES" (mapconcat #'identity files ", ")))))

;; (defun my/org-attach-visit-headline-from-dired ()
;;     "Go to the headline corresponding to this org-attach directory."
;;     (interactive)
;;     (let* ((id-parts (last (split-string default-directory "/" t) 2))
;;            (id (apply #'concat id-parts)))
;;       (let ((m (org-id-find id 'marker)))
;;         (unless m (user-error "Cannot find entry with ID \"%s\"" id))
;;         (pop-to-buffer (marker-buffer m))
;;         (goto-char m)
;;         (move-marker m nil)
;;         (org-fold-show-context))))

(defun my/org-attach-visit-headline-from-dired ()
    "Go to the headline corresponding to this org-attach directory."
    (interactive)
    (require 'org-attach)
    (let* ((path (replace-regexp-in-string (regexp-quote org-attach-directory) "" (expand-file-name (dired-filename-at-point))))
           (id-parts (split-string path "/"))
           (id1 (nth 1 id-parts))
           (id2 (nth 2 id-parts))
           (id (concat id1 id2)))
      (let ((m (org-id-find id 'marker)))
        (unless m (user-error "Cannot find entry with ID \"%s\"" id))
        (pop-to-buffer (marker-buffer m))
        (goto-char m)
        (move-marker m nil)
        (org-fold-show-context))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-'") #'my/org-attach-visit-headline-from-dired))

(use-package org-habit
  :after org
  :config
  (setq org-habit-graph-column 100)
  (add-to-list 'org-modules 'org-habit t))

(use-package org-id
  :after org
  :config
  (setq org-id-method 'ts)
  (setq org-id-link-to-org-use-id 'create-if-interactive))

;;;###autoload
(defun my/copy-idlink ()
  "Copy idlink to clipboard."
  (interactive)
  (when (eq major-mode 'org-agenda-mode) ;switch to orgmode
    ;; (org-agenda-show)
    (org-agenda-goto))
  (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
    (let* ((mytmphead (nth 4 (org-heading-components)))
           (mytmpid (funcall 'org-id-get-create))
           (mytmplink (format "- [ ] [[id:%s][%s]]" mytmpid mytmphead)))
      (kill-new mytmplink)
      (message "Copied %s to killring (clipboard)" mytmplink))))

(defun update-org-ids-in-directory (directory)
  "Update Org IDs in all Org files in DIRECTORY."
  (interactive "DEnter directory: ")
  (when (file-directory-p directory)
    (let ((org-files (directory-files-recursively directory "\\.org\\'")))
      (org-id-update-id-locations org-files t)
      (message "Updated Org IDs in %d files." (length org-files))))
  (unless (file-directory-p directory)
    (message "Not a valid directory: %s" directory)))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "<f8>") 'my/copy-idlink))

(use-package org-src
  :after org
  :config
  (setq org-src-window-setup 'current-window)
  (setq org-src-ask-before-returning-to-edit-buffer nil))

(use-package org-goto
  :after org
  :config
  (setq org-goto-interface 'outline-path-completion))

(use-package org-refile
  :commands org-refile
  :config
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-active-region-within-subtree t))

(use-package org-clock
  :commands org-clocking-p
  :bind ("C-c p i" . my/toggle-punch-in-or-out)
  :hook ((org-clock-in . my/afplay-clock-in)
         (org-clock-out . my/afplay-clock-out))
  :config
  (defun my/afplay-clock-in ()
    (async-shell-command "afplay /System/Library/Sounds/Ping.aiff"))
  (defun my/afplay-clock-out ()
    (async-shell-command "afplay /System/Library/Sounds/Basso.aiff"))

  (org-clock-persistence-insinuate)
  (setq org-clock-history-length 23)
  (setq org-clock-in-resume t)
  (setq org-clock-into-drawer "LOGCLOCK")
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-out-when-done t)
  (setq org-clock-persist t)
  (setq org-clock-clocktable-default-properties '(:maxlevel 5 :link t :tags t))
  (setq org-clock-persist-query-resume nil)
  (setq org-clock-report-include-clocking-task t)
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
  (defvar bh/default-task-id "20230912T181100.221961")
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
        (bh/punch-out)
      (bh/punch-in nil))))

(use-package org-indent
  :hook (org-mode . org-indent-mode))

(use-package ox-html
  :after ox
  :config
  (setq org-export-global-macros
        '(("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@")))
  (setq org-html-preamble t)
  (setq org-html-preamble-format
      '(("en" "<a href=\"/index.html\" class=\"button\">Home</a>
               <a href=\"/posts/index.html\" class=\"button\">Posts</a>
               <a href=\"/about.html\" class=\"button\">About</a>
               <hr>")))

  (setq org-html-postamble t)

  (setq org-html-postamble-format
        '(("en" "<hr><div class=\"info\"> <span class=\"created\">Created with %c on MacOS</span>
 <span class=\"updated\">Updated: %d</span> </div>")))

  (setq org-html-head-include-default-style nil)

  (setq org-html-head
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/style.css\" />
         <script src=\"js/copy.js\"></script> "))

(use-package ox-publish
  :after ox
  :config
  ;; https://git.sr.ht/~taingram/taingram.org/tree/master/item/publish.el
  (defun taingram--sitemap-dated-entry-format (entry style project)
    "Sitemap PROJECT ENTRY STYLE format that includes date."
    (let ((filename (org-publish-find-title entry project)))
      (if (= (length filename) 0)
          (format "*%s*" entry)
        (format "{{{timestamp(%s)}}}   [[file:%s][%s]]"
                (format-time-string "%Y-%m-%d"
                                    (org-publish-find-date entry project))
                entry
                filename))))

  (defvar my/publish-directory "~/Blogs/")

  (setq org-publish-project-alist
        `(("site"
           :base-directory ,website-directory
           :base-extension "org"
           :recursive nil
           :publishing-directory ,my/publish-directory
           :publishing-function org-html-publish-to-html)

          ("posts"
           :base-directory ,(expand-file-name "posts" website-directory)
           :base-extension "org"
           :publishing-directory ,(expand-file-name "posts" my/publish-directory)
           :publishing-function org-html-publish-to-html
           :with-author t
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "posts"
           :sitemap-sort-files anti-chronologically
           :sitemap-format-entry taingram--sitemap-dated-entry-format)

          ("static"
           :base-directory ,website-directory
           :base-extension "css\\|js\\|txt\\|jpg\\|gif\\|png"
           :recursive t
           :publishing-directory  ,my/publish-directory
           :publishing-function org-publish-attachment)

          ("personal-website" :components ("site" "posts" "static"))))

  (defun my/ox-publish-move-images (origin publish)
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[file:\\(.*?\\)\\]\\]" nil t)
        (let* ((image-path (match-string 1))
               (picture-name (car (last (split-string image-path "/"))))
               (new-path (concat my/publish-directory "static/" picture-name)))
          (copy-file image-path new-path t)))))

  (defun my/ox-publish-replace-src-path (origin publish)
    "Replace image paths in the HTML file."
    (interactive)
    (message "%s%s" origin publish)
    (with-temp-buffer
        (insert-file-contents publish)
        (goto-char (point-min))
        (while (re-search-forward "file:///Users/jousimies/Nextcloud/L.Personal.Galaxy/pictures/" nil t)
          (replace-match "../static/"))
        (write-region (point-min) (point-max) publish)))

  (add-hook 'org-publish-after-publishing-hook 'my/ox-publish-move-images)
  (add-hook 'org-publish-after-publishing-hook 'my/ox-publish-replace-src-path))

(provide 'init-org)
;;; init-org.el ends here.
