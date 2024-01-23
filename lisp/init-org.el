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
		  org-startup-folded 'content
		  org-image-actual-width nil
		  org-use-speed-commands t
		  org-highlight-latex-and-related '(latex script)
		  org-enforce-todo-dependencies t
		  org-enforce-todo-checkbox-dependencies t
		  org-tags-sort-function 'org-string-collate-greaterp
		  org-lowest-priority ?D
		  org-priority-default ?C
		  org-columns-default-format "%50ITEM %TODO %3PRIORITY %TAGS")

  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2)))

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
(with-eval-after-load 'org-capture
  (setq org-capture-templates
        '(("i" "GTD Inbox"
           entry (file (lambda () (concat mobile-document "iCloud~com~appsonthemove~beorg/Documents/org/inbox.org")))
           "* %?\n%U\n" :time-prompt t :tree-type week)
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

(defun my/org-capture-inbox ()
  (interactive)
  (org-capture t "i"))

;; org-capture
(with-eval-after-load 'org-attach
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

;; org-timer as pomodoro
(use-package org-timer
  :commands org-timer-pause-or-continue my/pomodoro-toggle
  :hook (org-timer-done . my/play-sound)
  :config
  (setopt org-timer-default-timer "25")
  (defun my/pomodoro-toggle ()
	(interactive)
	(require 'org-timer)
	(if org-timer-countdown-timer
		(org-timer-stop)
	  (org-timer-set-timer 25)))
  (defun my/play-sound ()
    (async-shell-command "afplay /System/Library/Sounds/Submarine.aiff")))

(use-package pomm-third-time
  :load-path "packages/pomm.el/"
  :commands pomm-third-time
  :hook (on-first-buffer . pomm-mode-line-mode)
  :config
  (setq pomm-audio-enabled t)
  (setq pomm-audio-tick-enabled t)
  (setq pomm-third-time-state-file-location (expand-file-name "pomm-third-time" cache-directory)))

;; org indent mode
(add-hook 'org-mode-hook #'org-indent-mode)

;; Third party packages related to org-mode
(use-package imenu-list
  :load-path "packages/imenu-list/"
  :commands imenu-list-minor-mode
  :config
  (set-face-attribute 'imenu-list-entry-face-0 nil
                      :foreground (face-foreground 'ef-themes-heading-1)
                      :inherit 'bold)
  (set-face-attribute 'imenu-list-entry-face-1 nil
                      :foreground (face-foreground 'ef-themes-heading-2)
                      :inherit 'bold)

  (setq imenu-list-position 'left)
  (setq-default imenu-list-mode-line-format nil))

(use-package olivetti
  :load-path "packages/olivetti/"
  :bind ("s-M-z" . olivetti-mode)
  :hook ((olivetti-mode-on . (lambda ()
                               (imenu-list-minor-mode 1)))
         (olivetti-mode-off . (lambda ()
                                (imenu-list-minor-mode -1)))))

(use-package form-feed
  :load-path "packages/form-feed/"
  :hook (org-mode . form-feed-mode))

;; Emacs 30 math-preview do now work as expected.
;; (use-package math-preview
;;   :load-path "packages/math-preview/"
;;   :commands math-preview-all math-preview-clear-all
;;   :hook (find-file . (lambda ()
;;                        (when (eq major-mode 'org-mode)
;;                          (auto/math-preview-all))))
;;   :config
;;   (setq math-preview-scale 1.1)
;;   (setq math-preview-raise 0.2)
;;   (setq math-preview-margin '(1 . 0))
;;   (add-to-list 'org-options-keywords "NO_MATH_PREVIEW:"))

;;;###autoloads
(defun auto/math-preview-all ()
  "Auto update clock table."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char 0)
      (unless (string-equal (cadar (org-collect-keywords '("NO_MATH_PREVIEW"))) "t")
        (when (re-search-forward "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}" (point-max) t)
          (math-preview-all))))))

;; https://emacs-china.org/t/topic/6601
;; pngpaste or below applescript
(defun org-insert-image (name)
  "insert a image from clipboard"
  (interactive "sName: ")
  (let* ((path (expand-file-name "pictures/" my-galaxy))
		 (image-file (concat path name ".png")))
	(do-applescript (concat
					 "set the_path to \"" image-file "\" \n"
					 "set png_data to the clipboard as «class PNGf» \n"
					 "set the_file to open for access (POSIX file the_path as string) with write permission \n"
					 "write png_data to the_file \n"
					 "close access the_file"))
	(insert (format "#+NAME: fig:%s\n#+CAPTION: %s\n" name name))
	(insert "#+ATTR_ORG: :width 500px\n#+ATTR_LATEX: :width 10cm :placement [!htpb]\n#+ATTR_HTML: :width 600px\n")
	(org-insert-link nil (concat "file:" image-file) "")))

(defun clipboard-contains-png-p ()
  "Check if the clipboard contains PNG data."
  (let ((script (concat
				 "set clipboardData to the clipboard as record\n"
				 "set clipboardType to (clipboard info for clipboardData) as string\n"
				 "if clipboardType contains \"PNG\" then\n"
				 "return \"true\"\n"
				 "end if\n"
				 )))
	(string-match "true" (shell-command-to-string (concat "osascript -e " (shell-quote-argument script))))))

(defun my/yank ()
  (interactive)
  (if (clipboard-contains-png-p)
      (call-interactively 'org-insert-image)
    (cond ((eq major-mode 'vterm-mode) (term-paste))
          (t (yank)))))

(global-set-key (kbd "s-v") #'my/yank)

;; Instead of using `C-c C-x C-v' to toggle display inline image.
;; pixel-scroll-precision-mode enabled.
;; (use-package org-imgtog
;;   :load-path "packages/org-imgtog/"
;;   :hook (org-mode . org-imgtog-mode))

(use-package plantuml
  :load-path "packages/plantuml-emacs/"
  :commands plantuml-org-to-mindmap-open plantuml-org-to-wbs-open
  :config
  (setq plantuml-jar-path
        (concat (string-trim
                 (shell-command-to-string "readlink -f $(brew --prefix plantuml)"))
                "/libexec/plantuml.jar")))

(defun org-export-docx ()
  "Convert org to docx."
  (interactive)
  (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
        (template-file (expand-file-name "template/template.docx" user-emacs-directory)))
    (shell-command (format "pandoc %s -o %s --reference-doc=%s" (buffer-file-name) docx-file template-file))
    (message "Convert finish: %s" docx-file)))

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
(use-package org-drawio
  :load-path "packages/org-drawio/"
  :commands (org-drawio-add
             org-drawio-open)
  :custom ((org-drawio-input-dir (abbreviate-file-name (expand-file-name "mindmap" my-galaxy)))
           (org-drawio-output-dir (abbreviate-file-name (expand-file-name "pictures" my-galaxy)))
           (org-drawio-output-page "0")
		   (org-drawio-command-drawio "/Applications/draw.io.app/Contents/MacOS/draw.io")
		   (org-drawio-command-pdf2svg "/opt/homebrew/bin/pdf2svg")
		   (org-drawio-crop t)))

(provide 'init-org)
;;; init-org.el ends here.
