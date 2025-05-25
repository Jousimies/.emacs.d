;; init-org.el --- Live in plain life with org-mode. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/packages/emacs-htmlize/")

(with-eval-after-load 'org
  (setopt org-ellipsis " ⇲"
	  org-modules '(org-habit)
	  org-imenu-depth 4
	  org-return-follows-link t
	  org-display-remote-inline-images 'download
	  org-log-into-drawer t
	  org-fast-tag-selection-single-key 'expert
	  org-adapt-indentation nil
	  org-fontify-quote-and-verse-blocks t
	  org-support-shift-select t
	  org-treat-S-cursor-todo-selection-as-state-change nil
	  org-hide-leading-stars nil
	  org-startup-with-inline-images t
	  org-image-actual-width nil
	  org-use-speed-commands t
	  org-highlight-latex-and-related '(latex script)
	  org-enforce-todo-dependencies t
	  org-enforce-todo-checkbox-dependencies t
	  org-export-allow-bind-keywords t
	  org-tags-sort-function 'org-string-collate-greaterp
	  org-lowest-priority ?D
	  org-priority-default ?C
	  org-columns-default-format "%50ITEM %TODO %3PRIORITY %TAGS"
	  org-persist-directory (expand-file-name "org-persist" cache-directory))

  (setopt org-preview-latex-default-process 'dvisvgm)
  (setopt org-format-latex-options (plist-put org-format-latex-options :scale 2)))

(with-eval-after-load 'org
  ;; (setopt org-todo-state-tags-triggers
  ;;         (quote (("CNCL" ("CNCL" . t))
  ;;                 ("WAIT" ("WAIT" . t))
  ;;                 (done ("WAIT"))
  ;;                 ("TODO" ("WAIT") ("CNCL"))
  ;;                 ("NEXT" ("WAIT") ("CNCL"))
  ;;                 ("DONE" ("WAIT") ("CNCL")))))
  (setopt org-todo-repeat-to-state t)
  (setopt org-todo-keywords
          '((sequence "NEXT(n)" "TODO(t)" "|" "WAIT(w@)" "CNCL(c@/!)" "DONE(d)"))))

(with-eval-after-load 'org-faces
  (setq org-todo-keyword-faces
        '(("TODO" . (:inherit (bold org-todo)))
          ("NEXT" . (:inherit (success org-todo)))
          ("CNCL" . (:inherit (shadow org-todo)))
          ("DONE" . (:inherit (button org-todo)))
          ("WAIT" . (:inherit (warning org-todo)))))
  (setq org-priority-faces
        '((?A . (bold . org-priority))
          (?B . org-priority)
          (?C . (shadow . org-priority)))))

(with-eval-after-load 'org-habit
  (setopt org-habit-graph-column 70))

;; ob-core
(with-eval-after-load 'ob-core
  (setq org-confirm-babel-evaluate nil)
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
  (advice-add 'org-babel-execute-src-block :before 'my/org-babel-execute-src-block))

;; org-capture
(global-set-key (kbd "<f12>") #'org-capture)
(with-eval-after-load 'org-capture
  (setq org-capture-templates
        `(("i" "Inbox"
           entry (file ,(concat icloud "iCloud~com~appsonthemove~beorg/Documents/org/inbox.org"))
           "* %?\n%U\n" :time-prompt t :tree-type week)
          ("l" "Inbox with link"
           entry (file ,(concat icloud "iCloud~com~appsonthemove~beorg/Documents/org/inbox.org"))
           "* %?\n %U\n%a\n" :time-prompt t :tree-type week)
	  ("r" "Review"
           plain
           (file+olp+datetree ,(expand-file-name (format-time-string "logs/weekly_review_%Y.org") my-galaxy))
           (file "~/.emacs.d/template/review-weekly")
           :tree-type week :jump-to-captured t))))

;; org-attach
(defun org-attach-save-file-list-to-property (dir)
    "Save list of attachments to ORG_ATTACH_FILES property."
    (when-let* ((files (org-attach-file-list dir)))
      (org-set-property "ORG_ATTACH_FILES" (mapconcat #'identity files ", "))))
(add-hook 'org-attach-after-change-hook #'org-attach-save-file-list-to-property)

(defun update-org-attach-property ()
  "Manually update the ORG_ATTACH_FILES property for the current Org entry."
  (interactive)
  (let* ((dir (org-attach-dir t))
         (files (org-attach-file-list dir)))
    (when (and dir files)
      (org-with-wide-buffer
       (org-set-property "ORG_ATTACH_FILES" (mapconcat #'identity files ", ")))
      (message "ORG_ATTACH_FILES property updated."))))

(with-eval-after-load 'org-attach
  (setopt org-attach-expert t
	  org-attach-id-dir (expand-file-name "attach" my-galaxy)
	  org-attach-id-to-path-function-list '(org-attach-id-ts-folder-format
						org-attach-id-uuid-folder-format)))

;; org-id
(with-eval-after-load 'org-id
  (setopt org-id-method 'ts
	  org-id-locations-file (expand-file-name ".org-id-locations" cache-directory)
	  org-id-link-to-org-use-id 'create-if-interactive))

(defun update-org-ids-in-directory (directory)
  "Update Org IDs in all Org files in DIRECTORY."
  (interactive "DEnter directory: ")
  (require 'org-id)
  (when (file-directory-p directory)
    (let ((org-files (directory-files-recursively directory "\\.org\\'")))
      (org-id-update-id-locations org-files t)
      (message "Updated Org IDs in %d files." (length org-files))))
  (unless (file-directory-p directory)
    (message "Not a valid directory: %s" directory)))

(defun my/copy-idlink ()
  "Copy idlink to clipboard."
  (interactive)
  (when (eq major-mode 'org-agenda-mode) ;switch to orgmode
    ;; (org-agenda-show)
    (org-agenda-goto))
  (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
    (let* ((mytmphead (nth 4 (org-heading-components)))
           (mytmpid (funcall 'org-id-get-create))
           (mytmplink (format "[[id:%s][%s]]" mytmpid mytmphead)))
      (kill-new mytmplink)
      (message "Copied %s to killring (clipboard)" mytmplink))))

(defun jf/org-link-remove-link ()
  "Remove the link part of an `org-mode' link at point and keep only the description."
  (interactive)
  (let ((elem (org-element-context)))
    (when (eq (car elem) 'link)
      (let* ((content-begin (org-element-property :contents-begin elem))
             (content-end  (org-element-property :contents-end elem))
             (link-begin (org-element-property :begin elem))
             (link-end (org-element-property :end elem)))
        (when (and content-begin content-end)
          (let ((content (buffer-substring-no-properties content-begin content-end)))
            (delete-region link-begin link-end)
            (insert content)))))))

;; org-src
(with-eval-after-load 'org-src
  (setopt org-src-window-setup 'current-window
	  org-src-ask-before-returning-to-edit-buffer nil))

;; org-goto
(with-eval-after-load 'org-goto
  (setq org-goto-interface 'outline-path-completion))

;; org-refile
(with-eval-after-load 'org-refile
  (setopt org-refile-targets '((nil :maxlevel . 9)
                               (org-agenda-files :maxlevel . 9))
	  org-refile-use-outline-path t
	  org-outline-path-complete-in-steps nil
	  org-refile-allow-creating-parent-nodes 'confirm
	  org-refile-use-outline-path 'file
	  org-refile-active-region-within-subtree t))

;; org-clock
(with-eval-after-load 'org-clock
  (org-clock-persistence-insinuate)
  (setopt org-clock-persist-file (expand-file-name "org-clock-save.el" cache-directory)
	  org-clock-history-length 23
	  org-clock-in-resume t
	  org-clock-into-drawer "LOGCLOCK"
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

;; org indent mode
;; (add-hook 'org-mode-hook #'org-indent-mode)

;; Yank media
(with-eval-after-load 'org
  (setopt org-yank-image-file-name-function 'org-yank-image-read-filename))

(with-eval-after-load 'org
 (define-key org-mode-map (kbd "C-c C-v") #'yank-media))

(with-eval-after-load 'org-agenda
  (setopt org-agenda-window-setup 'other-tab
	  org-agenda-skip-scheduled-if-done t
	  org-agenda-skip-deadline-if-done t
	  org-agenda-todo-ignore-scheduled 'future
	  org-agenda-todo-ignore-deadlines 'near
	  org-agenda-dim-blocked-tasks t
	  org-agenda-compact-blocks t
	  org-agenda-align-tags-to-column 120
	  org-deadline-warning-days 7)
  (add-to-list 'org-agenda-custom-commands
	       '("b" "Book Shelf"
		 ((tags "+BookShelf"
			((org-agenda-prefix-format " %i")
			 (org-agenda-overriding-header "Reading Lists")))))))


(provide 'init-org)
;;; init-org.el ends here.
