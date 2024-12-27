;; init-org.el --- Live in plain life with org-mode. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

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
		  ;; org-startup-folded 'content
		  org-image-actual-width nil
		  org-use-speed-commands t
		  org-highlight-latex-and-related '(latex script)
		  org-enforce-todo-dependencies t
		  org-enforce-todo-checkbox-dependencies t
		  org-export-allow-bind-keywords t
		  org-tags-sort-function 'org-string-collate-greaterp
		  org-lowest-priority ?D
		  org-priority-default ?C
		  org-columns-default-format "%50ITEM %TODO %3PRIORITY %TAGS")
  ;; (set-face-attribute 'org-table nil :family "Sarasa Mono SC")
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2)))

;; (use-package math-preview
;;   :custom
;;   (math-preview-command "/opt/homebrew/bin/math-preview")
;;   (math-preview-scale 1.1)
;;   (math-preview-raise 0.2)
;;   (math-preview-margin '(1 . 0)))
;; (use-package org-xlatex
;;   :hook (org-mode . org-xlatex-mode))

(use-package cdlatex
  :load-path "packages/cdlatex/"
  :hook ((org-mode . org-cdlatex-mode)
         (LaTeX-mode . turn-on-cdlatex))
  :custom
  (cdlatex-auto-help-delay 0))

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
(global-set-key (kbd "<f11>") #'org-capture)
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

(defun my/org-capture-inbox ()
  (interactive)
  (org-capture t "i"))

(defun my/org-capture-review ()
  (interactive)
  (org-capture t "r"))

(defun my/org-capture-inbox-with-link ()
  (interactive)
  (org-capture t "l"))

(defun my/org-capture-work-clock ()
  (interactive)
  (org-capture t "wc"))

(defun my/org-capture-work-date ()
  (interactive)
  (org-capture t "wp"))

;; org-attach
;;;###autoload
(defun update-org-attach-property ()
  "Manually update the ORG_ATTACH_FILES property for the current Org entry."
  (interactive)
  (let* ((dir (org-attach-dir t))
         (files (org-attach-file-list dir)))
    (when (and dir files)
	  (org-with-wide-buffer
	   (org-set-property "ORG_ATTACH_FILES" (mapconcat #'identity files ", ")))
	  (message "ORG_ATTACH_FILES property updated."))))

(with-eval-after-load 'org
  (require 'org-attach)
  (setopt org-attach-expert t
		  org-attach-id-dir (expand-file-name "attach" my-galaxy)
		  org-attach-id-to-path-function-list '(org-attach-id-ts-folder-format
												org-attach-id-uuid-folder-format))
  (defun org-attach-save-file-list-to-property (dir)
    "Save list of attachments to ORG_ATTACH_FILES property."
    (when-let* ((files (org-attach-file-list dir)))
      (org-set-property "ORG_ATTACH_FILES" (mapconcat #'identity files ", "))))
  (add-hook 'org-attach-after-change-hook #'org-attach-save-file-list-to-property))

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
  (setq org-src-window-setup 'current-window)
  (setq org-src-ask-before-returning-to-edit-buffer nil))

;; org-goto
(with-eval-after-load 'org-goto
  (setq org-goto-interface 'outline-path-completion))

;; org-refile
(with-eval-after-load 'org-refile
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-active-region-within-subtree t))

;; org-clock
(with-eval-after-load 'org-clock
  (org-clock-persistence-insinuate)
  (setq org-clock-persist-file (expand-file-name "org-clock-save.el" cache-directory))
  (setopt org-clock-history-length 23
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

;; org-indent-mode hide leading stars, sometimes cursor become invisible.
(use-package org-superstar
  :load-path "packages/org-superstar-mode/"
  :hook ((org-mode . org-superstar-mode)
		 (org-superstar-mode . org-indent-mode))
  :custom
  (org-hide-leading-stars t)
  (org-superstar-headline-bullets-list '("󰼏" "󰼐" "󰼑" "󰼒" "󰼓" "󰼔" "󰼕")))

;; 2024-09-14 还是放弃使用 org-modern，并没有太好用
;; org-modern can replace org-superstar
;; (use-package org-modern
;;   :hook (org-mode . global-org-modern-mode)
;;   :custom
;;   ;; (org-modern-checkbox nil)
;;   (org-modern-star 'replace)
;;   (org-modern-replace-stars "󰼏󰼐󰼑󰼒󰼓󰼔󰼕")
;;   (org-modern-table nil)
;;   ;; (org-modern-footnote nil)
;;   ;; (org-modern-internal-target nil)
;;   ;; (org-modern-radio-target nil)
;;   ;; (org-modern-progress nil)
;;   ;; (org-modern-tag nil)
;;   (org-modern-block-fringe nil)
;;   (org-modern-block-name nil)
;;   ;; (org-modern-timestamp nil)
;;   (org-modern-horizontal-rule nil)
;;   (org-modern-list '((?+ . "+")
;;                      (?- . "-")
;;                      (?* . "*")))
;;   (org-modern-checkbox '((?X . "󰄸")
;;                          (?- . "󱅶")
;;                          (?\s . "󰄶"))))

;; Third party packages related to org-mode
(use-package imenu-list
  :load-path "packages/imenu-list/"
  :hook (imenu-list-major-mode . (lambda ()
                                   (setq-local truncate-lines t)))
  :custom
  (imenu-list-position 'left)
  (imenu-list-mode-line-format '("%e" my/winum)))

(defun my/toggle-visual-fill-center-imenu ()
  (interactive)
  (unless (featurep 'visual-fill-column)
    (require 'visual-fill-column))
  (setq visual-fill-column-center-text t)
  (if (or visual-fill-column-mode imenu-list-minor-mode)
      (progn
	(visual-fill-column-mode -1)
	(imenu-list-minor-mode -1))
    (progn
      (visual-fill-column-mode)
      (imenu-list-minor-mode))))
(global-set-key (kbd "s-M-z") #'my/toggle-visual-fill-center-imenu)

(use-package form-feed
  :load-path "packages/form-feed/"
  :hook ((org-mode . form-feed-mode)
		 (emacs-news-mode . form-feed-mode)))

;; (use-package org-xlatex
;;   :load-path "packages/org-xlatex/"
;;   :hook (org-mode . org-xlatex-mode))

;; Yank media
(with-eval-after-load 'org
  (setopt org-yank-image-file-name-function 'org-yank-image-read-filename))

(use-package yank-media
  :after org
  :bind (:map org-mode-map
			  ("C-c C-v" . yank-media)))

;; Instead of using `C-c C-x C-v' to toggle display inline image.
;; pixel-scroll-precision-mode enabled.
;; (use-package org-imgtog
;;   :load-path "packages/org-imgtog/"
;;   :hook (org-mode . org-imgtog-mode))

;; ox-pandoc
;; (with-eval-after-load 'ox
;;   (add-to-list 'load-path "~/.emacs.d/packages/ox-pandoc/")
;;   (add-to-list 'load-path "~/.emacs.d/packages/ht.el/")
;;   (require 'ox-pandoc))

(use-package plantuml
  :load-path "packages/plantuml-emacs/"
  :commands plantuml-org-to-mindmap-open plantuml-org-to-wbs-open)

(with-eval-after-load 'plantuml
  (setq plantuml-jar-path
        (concat (string-trim
                 (shell-command-to-string "readlink -f $(brew --prefix plantuml)"))
                "/libexec/plantuml.jar")))

(defun org-export-docx (input csl)
  (interactive "FInput file (Default is Buffer File):\nFCSL file (Default is chinese-gb7714-2005-numeric):")
  (let* ((base (or (file-name-sans-extension input) (buffer-file-name)))
		 (csl (or (expand-file-name "csl/chinese-gb7714-2005-numeric.csl" user-emacs-directory)))
		 (output (concat base ".docx")))
	(shell-command (format "pandoc %s -o %s --citeproc --csl %s" input output csl))))

;; https://www.reddit.com/r/emacs/comments/yjobc2/comment/iur16c7/
(defun nf/parse-headline (x)
  (plist-get (cadr x) :raw-value))

(defun nf/get-headlines ()
  (org-element-map (org-element-parse-buffer) 'headline #'nf/parse-headline))

(defun nf/link-to-headline ()
  "Insert an internal link to a headline."
  (interactive)
  (let* ((headlines (nf/get-headlines))
         (choice (completing-read "Headings: " headlines nil t))
         (desc (read-string "Description: " choice)))
    (org-insert-link buffer-file-name (concat "*" choice) desc)))

(use-package advance-words-count
  :load-path "packages/advance-words-count.el/"
  :bind ("M-=" . advance-words-count))

(defun my/insert-specified-datetree ()
  "Insert a datetree entry for a specified date."
  (interactive)
  (let* ((date (org-parse-time-string (org-read-date)))
         (year (nth 5 date))
         (month (nth 4 date))
         (day (nth 3 date)))
    (org-datetree-find-date-create (list month day year))
	(open-line 1)
	(forward-line 1)))

;; org-drawio
;; (use-package org-drawio
;;   :load-path "packages/org-drawio/"
;;   :commands (org-drawio-add
;;              org-drawio-open)
;;   :custom ((org-drawio-input-dir (abbreviate-file-name (expand-file-name "mindmap" my-galaxy)))
;;            (org-drawio-output-dir (abbreviate-file-name (expand-file-name "pictures" my-galaxy)))
;;            (org-drawio-output-page "0")
;; 		   (org-drawio-command-drawio "/Applications/draw.io.app/Contents/MacOS/draw.io")
;; 		   (org-drawio-command-pdf2svg "/opt/homebrew/bin/pdf2svg")
;; 		   (org-drawio-crop t)))

;; chatu
(use-package chatu
  :load-path "packages/chatu/"
  :hook (org-mode . chatu-mode)
  :commands (chatu-add
             chatu-open)
  :custom ((chatu-input-dir (abbreviate-file-name (expand-file-name "mindmap" my-galaxy)))
           (chatu-output-dir (abbreviate-file-name (expand-file-name "pictures" my-galaxy)))))

;;;###autoload
(defun my/org-chatu ()
  "Insert drawio image into org file with completion for knowledge name."
  (interactive)
  (let* ((folder (abbreviate-file-name (expand-file-name "mindmap" my-galaxy)))
         (files (directory-files folder nil "\\.drawio\\'"))
         (selected (completing-read "Choose Mindmap: " files))
         (name selected))
    (insert (format "#+chatu: :drawio \"%s\" :crop :nopdf\n" name))))

;; 没啥必要,纯文本的挺好
;; (use-package org-tag-beautify
;;   :hook (org-mode . org-tag-beautify-mode))

;; The contacts app in Phone is far better
;; (use-package org-contacts
;;   :custom
;;   (org-contacts-files `(,(expand-file-name "people/contacts.org" my-galaxy))))

;; edraw in Emacs
;; (add-to-list 'load-path "~/.emacs.d/packages/el-easydraw/")
;; (with-eval-after-load 'org
;;   (require 'edraw-org)
;;   (edraw-org-setup-default))
;; (use-package org-supertag
;;   :load-path "~/.emacs.d/packages/org-supertag/" "~/.emacs.d/packages/ht.el/"
;;   :hook (org-mode . org-supertag-setup))


(provide 'init-org)
;;; init-org.el ends here.
