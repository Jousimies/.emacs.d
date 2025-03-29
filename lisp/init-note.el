;; init-note.el.el --- Note taking. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:



(use-package denote
  :load-path "packages/denote/"
  :bind ((:map dired-mode-map
               ("r" . denote-dired-rename-marked-files-with-keywords)))
  :hook ((dired-mode . denote-dired-mode-in-directories)
	 (org-mode . denote-rename-buffer-mode))
  :custom
  (denote-rename-confirmations nil)
  (denote-org-store-link-to-heading nil)
  (denote-prompts '(title keywords subdirectory signature))
  (denote-directory (expand-file-name "denote" my-galaxy))
  (denote-file-name-slug-functions '((title . denote-sluggify-title)
				     (signature . denote-sluggify-signature)
				     (keyword . identity)))
  (denote-dired-directories
   (list denote-directory
         (thread-last denote-directory (expand-file-name "books"))
         (thread-last denote-directory (expand-file-name "outline"))
         (thread-last denote-directory (expand-file-name "literature"))
         (thread-last denote-directory (expand-file-name "term"))
         (thread-last denote-directory (expand-file-name "references"))))
  (denote-rename-buffer-format "%b %t")
  (denote-rename-buffer-backlinks-indicator ""))

(defun my/modus-themes-denote-faces (&rest _)
  (modus-themes-with-colors
    (custom-set-faces
     `(denote-faces-year ((,c :foreground ,cyan)))
     `(denote-faces-month ((,c :foreground ,magenta-warmer)))
     `(denote-faces-day ((,c :foreground ,cyan)))
     `(denote-faces-time-delimiter ((,c :foreground ,fg-main)))
     `(denote-faces-hour ((,c :foreground ,magenta-warmer)))
     `(denote-faces-minute ((,c :foreground ,cyan)))
     `(denote-faces-second ((,c :foreground ,magenta-warmer))))))

(add-hook 'ns-system-appearance-change-functions #'my/modus-themes-denote-faces)

(use-package denote-org
  :load-path "packages/denote-org/"
  :after denote)

(use-package denote-sequence
  :load-path "packages/denote-sequence/"
  :after denote
  :custom
  (denote-sequence-scheme 'alphanumeric))

;; A simple HACK to let denote support orderless
;; https://github.com/protesilaos/denote/issues/253
;; #+BEGIN: denote-files :regexp "ol: _tag1 _tag2 !boring"
;; #+END
;; (defun denote-orderless--is-orderless-filter (str)
;;   "Check whether `str' is an orderless filter and return the filter if it is, otherwise return nil."
;;   (let ((prefix "ol: "))
;;     (when (s-prefix? "ol: " str)
;;       (s-chop-left (length prefix) str))))

;; (defun denote-orderless-directory-files (oldfun &optional filter omit-current text-only)
;;   "Use orderless to filter files."
;;   (if-let ((ol-filter (denote-orderless--is-orderless-filter filter)))
;;       (let ((files (denote--directory-get-files)))
;;         (when (and omit-current buffer-file-name (denote-file-has-identifier-p buffer-file-name))
;;           (setq files (delete buffer-file-name files)))
;;         (when ol-filter
;;           (setq files (orderless-filter ol-filter files)))
;;         (when text-only
;;           (setq files (seq-filter #'denote-file-is-note-p files)))
;;         files)
;;     (funcall oldfun filter omit-current text-only)))

;; (advice-add 'denote-directory-files :around #'denote-orderless-directory-files)
;; (with-eval-after-load 'org
;;   (defun zyd-expand-and-complete-with-denote ()
;; 	(interactive)
;; 	(let ((limit (- (point) 2)))
;;       (when (looking-back "((" limit)
;; 		(progn
;;           (call-interactively #'denote-link-or-create)
;;           (let ((end-of-link (point)))
;; 			(goto-char limit)
;; 			(delete-char 2)
;; 			(goto-char end-of-link))))))

;;   (defun zyd-try-to-complete-then-cycle (&optional arg)
;; 	(interactive)
;; 	(zyd-expand-and-complete-with-denote)
;; 	(org-cycle arg))

;;   (define-key org-mode-map (kbd "<tab>") #'zyd-expand-and-complete-with-denote))

(defun find-file-other-window-no-jump (filename)
  "Find file in other window without jumping to that window."
  (interactive "FFind file in other window: ")
  (let ((current-window (selected-window)))
    (find-file-other-window filename)
    (select-window current-window)))

;;;###autoload
(defun my/denote-find-link-other-window ()
  "Use minibuffer completion to visit linked file."
  (declare (interactive-only t))
  (interactive)
  (find-file-other-window-no-jump
   (denote-link--find-file-prompt
    (or (denote-link-return-links)
        (user-error "No links found")))))

;;;###autoload
(defun my/denote-sequence-find-dired (type)
  "Find relatives of current file based on RELATIVE-TYPE.
RELATIVE-TYPE can be 'all-parents, 'parent, 'all-children, or 'children."
  (interactive)
  (if-let* ((sequence (denote-sequence-file-p buffer-file-name))
            (default-directory (denote-directory))
            (relatives (delete buffer-file-name
                               (ensure-list
                                (denote-sequence-get-relative sequence type))))
            (files-sorted (denote-sequence-sort-files relatives)))
      (dired (cons (format-message "*`%s' type relatives of `%s'" type sequence)
                   (mapcar #'file-relative-name files-sorted)))
    (user-error "The sequence `%s' has no relatives of type `%s'" sequence type)))

;;;###autoload
(defun denote-sequence-find-dired-all-parents ()
  "Find all parents of current file."
  (interactive)
  (my/denote-sequence-find-dired 'all-parents))

;;;###autoload
(defun denote-sequence-find-dired-parent ()
  "Find parent of current file."
  (interactive)
  (my/denote-sequence-find-dired 'parent))

;;;###autoload
(defun denote-sequence-find-dired-all-children ()
  "Find all children of current file."
  (interactive)
  (my/denote-sequence-find-dired 'all-children))

;;;###autoload
(defun denote-sequence-find-dired-children ()
  "Find children of current file."
  (interactive)
  (my/denote-sequence-find-dired 'children))

(defun denote-sequence-find-dired-siblings ()
  "Find siblings of current file."
  (interactive)
  (my/denote-sequence-find-dired 'siblings))

(use-package consult-denote
  :load-path "packages/consult-denote/"
  :hook (org-mode . consult-denote-mode))

(use-package consult-notes
  :load-path "packages/consult-notes/"
  :commands consult-notes
  :config
  (defun my/consult-notes--file-dir-annotate (name dir cand)
    "Annotate file CAND with its directory DIR, size, and modification time."
    (let* ((file  (concat (file-name-as-directory dir) cand))
           (dirs  (abbreviate-file-name dir))
           (attrs (file-attributes file))
           (fsize (file-size-human-readable (file-attribute-size attrs)))
	   (ftime (consult-notes--time (file-attribute-modification-time attrs))))
      (put-text-property 0 (length name)  'face 'consult-notes-name name)
      (put-text-property 0 (length fsize) 'face 'consult-notes-size fsize)
      (put-text-property 0 (length ftime) 'face 'consult-notes-time ftime)
      (format "%7s %8s  %12s" name fsize ftime)))
  (setq consult-notes-file-dir-annotate-function 'my/consult-notes--file-dir-annotate)
  (setq consult-notes-file-dir-sources
        `(("Articles"  ?a  ,(concat my-galaxy "/blogs_source/posts"))
          ("Denote Notes"  ?d ,(expand-file-name "denote" my-galaxy))
          ("Terminology"  ?t ,(expand-file-name "denote/term" my-galaxy))
          ("Book Reading"  ?b ,(expand-file-name "denote/books" my-galaxy))
          ("Knowledge"  ?k ,(expand-file-name "denote/knowledge" my-galaxy))
          ("Meet"  ?m ,(expand-file-name "meeting" my-galaxy))
          ("References"  ?r ,(expand-file-name "denote/references" my-galaxy))
          ("Literature"  ?l ,(expand-file-name "denote/literature" my-galaxy))
          ("Journal"  ?j ,(expand-file-name "denote/journal" my-galaxy)))))

;; Sometimes I want open the web archive file with eww.
(defun my/org-get-link-under-point ()
  "Get the link under the point in Org mode."
  (let* ((link (org-element-lineage (org-element-context) '(link) t)))
    (if link
	(org-element-property :raw-link link)
      (url-get-url-at-point))))

(defun my/open-link-with-eww ()
  (interactive)
  (when-let ((link (my/org-get-link-under-point)))
    (if (org-file-url-p link)
	(org-open-at-point)
      (eww (concat "file://" (expand-file-name link))))))

(use-package denote-sort
  :commands denote-sort-dired
  :bind (:map dired-mode-map
	      ("/ r" . my/denote-sort-regexp))
  :config

  (defun my/denote-signature-retrieve ()
    (let* ((file (or (buffer-file-name) (dired-get-filename))))
      (when file
	(denote-retrieve-filename-signature file))))

  (defun my/denote-sort-regexp (regexp)
    (interactive (list
		  (read-regexp
		   (concat "Files matching PATTERN" (format " (default: %s)" (my/denote-signature-retrieve)) ": ")
		   (my/denote-signature-retrieve)
		   nil)))
    (denote-sort-dired (concat "==" regexp) 'signature nil nil))

  (defun my/denote-sort-with-days ()
    "Sort files by the past week's period using denote."
    (interactive)
    (let ((regexp (call-interactively 'my/denote-period-week)))
      (denote-sort-dired regexp 'signature nil nil))))

;; fliter denote create by days ago
(defun my/denote-period (&optional days)
  "Generate a regular expression for dates from a specified period ago to today.
DAYS is the optional number of days ago, defaulting to 7."
  (interactive)
  (let* ((days (or days 6))  ;; 使用 days 参数，如果未指定则默认为 7 天
         (current-time (current-time))
         (ago-date-time (time-subtract current-time (days-to-time days)))
         (current-date (format-time-string "%Y-%m-%d" current-time))
         (ago-date (format-time-string "%Y-%m-%d" ago-date-time))
         (cur-year (substring current-date 0 4))
         (cur-month (substring current-date 5 7))
         (cur-day (string-to-number (substring current-date 8 10)))
         (ago-year (substring ago-date 0 4))
         (ago-month (substring ago-date 5 7))
         (ago-day (string-to-number (substring ago-date 8 10))))

    (if (string= cur-year ago-year)
        (if (string= cur-month ago-month)
            (format "^%s%s[%02d-%02d]" cur-year cur-month ago-day cur-day)
          (format "^%s\\(%s[%02d-31]\\|%s[01-%02d]\\)"
                  cur-year
                  ago-month ago-day
                  cur-month cur-day))
      (format "\\(^%s12[%02d-31]\\|^%s01[01-%02d]\\)"
              ago-year ago-day
              cur-year cur-day))))

(defun my/denote-sort-period-week ()
  (interactive)
  (denote-sort-dired (my/denote-period) nil nil nil))

(defun my/denote-sort-sigature-lv1 ()
  (interactive)
  (let ((regexp (call-interactively 'my/denote-sort-lv-1)))
    (denote-sort-dired regexp 'signature nil nil)))

(defun my/denote-sort-sigature-lv2 ()
  (interactive)
  (let ((regexp (call-interactively 'my/denote-sort-lv-2)))
    (denote-sort-dired regexp 'signature nil nil)))

(defun my/denote-sort-lv-2 (lv)
  (interactive "nInput the Level of Signature(1-7): ")
  (format "\\(==%s[a-z]-\\)" lv))

(defun my/denote-sort-lv-1 ()
  (interactive)
  (format "\\(==[0-9]-\\)"))

(defun my/literature-entry (url title keywords file-path file-new-path)
  "Save a literature entry and add it to the 'literature' denote database."
  (split-window-right)
  (rename-file file-path file-new-path)
  (denote title
          keywords
          'org
          (expand-file-name "literature" (denote-directory))
          nil)
  (save-excursion
    (goto-char (point-max))
    (insert "* ")
    (org-insert-link nil file-new-path title)
    (org-set-property "URL" url)
    (org-set-tags "Reference")
    (beginning-of-line)
    (while (re-search-forward (expand-file-name "~") nil t 1)
      (replace-match "~" t nil))))

(defun my/literature-save-from-safari ()
  "Create an `literature' denote entry from Safari page."
  (interactive)
  (let* ((url (car (grab-mac-link-safari-1)))
         (title (cadr (grab-mac-link-safari-1)))
         (keywords (denote-keywords-prompt))
         (ID (format-time-string "%Y%m%dT%H%M%S"))
         (new-title (concat ID "--" title))
         (file-path (concat my-web_archive title ".html"))
         (file-new-path (concat my-web_archive new-title ".html")))
    (if (not (file-exists-p file-path))
        (message "Please save webpage first!!!")
      (my/literature-entry url title keywords file-path file-new-path))))

(defun my/literature-save-from-xwidget ()
  "Create a `literature' denote entry from xwidget."
  (interactive)
  (let* ((url (xwidget-webkit-uri (xwidget-webkit-current-session)))
         (title (xwidget-webkit-title (xwidget-webkit-current-session)))
         (keywords (denote-keywords-prompt))
         (ID (format-time-string "%Y%m%dT%H%M%S"))
         (new-title (concat ID "--" title))
         (file-path (concat my-web_archive title ".html"))
         (file-new-path (concat my-web_archive new-title ".html")))
    (if (not (file-exists-p file-path))
        (when (my/save-xwidget-to-webarchive)
          (my/literature-entry url title keywords file-path file-new-path))
      (my/literature-entry url title keywords file-path file-new-path))))

(defun my/literature-save ()
  "Save a literature entry from either Safari or xwidget-webkit."
  (interactive)
  (if (string-prefix-p "*xwidget" (buffer-name))
      (my/literature-save-from-xwidget)
    (my/literature-save-from-safari)))

(use-package ibooks-annot
  :load-path "packages/ibooks-annot.el/"
  :commands ibooks-annot/extract-annotations-to-note ibooks-annot/open-book-with-ibooks
  :config
  (setq pdfannots-script "~/.emacs.d/packages/pdfannots/pdfannots.py -f json")
  (setq ibooks-annot/book-note-directory (expand-file-name "denote/books" my-galaxy)))

(use-package denote-explore
  :load-path "packages/denote-explore/"
  :commands (denote-explore-count-notes
	     denote-explore-count-keywords
	     denote-explore-keywords-barchart
	     denote-explore-identify-duplicate-identifiers
	     denote-explore-rename-keyword)
  :config
  (setq denote-explore-network-filename (expand-file-name "mindmap/denote-network.html" my-galaxy))
  (setq denote-explore-json-edges-filename (expand-file-name "denote-edges.json" cache-directory))
  (setq denote-explore-json-vertices-filename (expand-file-name "denote-vertices.json" cache-directory)))

;;;###autoload
(defun my/denote-info ()
  "Count number of Denote text files,keywords and attachments."
  (interactive)
  (let* ((all-files (length (denote-directory-files)))
	 (denote-files (length (denote-directory-files nil nil t)))
	 (attachments (- all-files denote-files))
	 (keywords (length (denote-keywords))))
    (message "%s Denote files (%s Attachments), %s Distinct Keywords."
	     denote-files attachments keywords)))

(use-package citar-denote
  :load-path "packages/citar-denote/"
  :after citar
  :commands (citar-denote-dwim
	     citar-denote-open-reference-entry
	     citar-denote-find-reference
	     citar-denote-find-citation
	     citar-denote-add-citekey
	     citar-denote-remove-citekey)
  ;; :hook (on-first-input . citar-denote-mode)
  :config
  (citar-denote-mode)
  (setq citar-denote-use-bib-keywords t)
  (setq citar-denote-subdir t))

;;;###autoload
(defun citar-denote-open-files ()
  "Open attachment with a bibliographic reference.

When more than one bibliographic item is referenced, select item first."
  (interactive)
  ;; Any citation keys in the note?
  (if-let* ((keys (citar-denote--retrieve-references (buffer-file-name)))
            (key (if (= (length keys) 1)
                     (car keys)
                   (citar-select-ref
                    :filter (citar-denote--has-citekeys keys)))))
      (citar-open-files key)
    (if (denote-file-is-note-p (buffer-file-name))
        (when (yes-or-no-p "Current buffer does not reference a citation key.  Add a reference? ")
          (citar-denote-add-citekey)
          (citar-denote-dwim))
      (user-error "Buffer is not a Denote file"))))

(defun my/new-blog (title)
  (interactive "sTitle: ")
  (let ((filename (format "%s" title))
        (ext ".org"))
    (find-file (concat website-directory "posts/" filename ext))
    (insert "#+TITLE: " title "\n")
    (tempel-insert 'blog)))

(defun my/new-meeting (meet)
  (interactive "sTitle: ")
  (let ((filename (format "%s-%s" (format-time-string "%Y%m%d") meet))
        (ext ".org"))
    (find-file (concat my-galaxy "/meeting/" filename ext))
    (insert "#+TITLE: " meet "\n")
    (tempel-insert 'meeting)))

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

;; https://github.com/zaeph/.emacs.d/blob/615ac37be6bd78c37e967fdb43d28897a4116583/lisp/zp-org.el#L194
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
it can be passed in POS."

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

;; https://github.com/zaeph/.emacs.d/blob/615ac37be6bd78c37e967fdb43d28897a4116583/lisp/zp-org.el#L212
(defun my/org-set-date ()
  "Update the LAST_MODIFIED file property in the preamble."
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

(defun my/gtd-and-archive-list ()
  "Append all file names in `org-gtd-directory' to a list."
  (unless (featurep 'org-gtd)
    (require 'org-gtd))
  (let ((file-list '()))
    (dolist (file (directory-files org-gtd-directory t))
      (when (and (file-regular-p file) (not (member (file-name-nondirectory file) '("." ".."))))
        (setq file-list (append file-list (list file)))))
    file-list))

;; Need to install macosrec.
;; https://github.com/xenodium/macosrec
(defun my/ocr ()
  (interactive)
  (shell-command "macosrec --ocr --clipboard"))

;; Apple Notes
(defun create-apple-note (note-title note-content)
  "Create a new Apple Note with the given title and content."
  (let ((script
         (format
          "set noteTitle to %S
           set noteContent to %S
           tell application \"Notes\" to tell account \"iCloud\"
               set theNote to make new note at folder \"Notes\" with properties {name:noteTitle, body:noteContent}
           end tell
           activate application \"Notes\""
          note-title note-content)))
    (start-process "AppleScript" nil "osascript" "-e" script)))

(defun prompt-for-note-title ()
  "Prompt the user to enter the title for the note using minibuffer."
  (read-string "Enter note title: "))

(defun create-apple-note-with-title ()
  "Create a new Apple Note with a title provided by the user."
  (interactive)
  (let* ((title (prompt-for-note-title))
         (content (if (region-active-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    "")))
    (create-apple-note title content)))

;; (use-package spacious-padding
;;   :load-path "~/.emacs.d/packages/spacious-padding/"
;;   :hook (org-mode . spacious-padding-mode)
;;   :custom
;;   (spacious-padding-subtle-mode-line t)
;;   (spacious-padding-widths
;;       '( :internal-border-width 30
;;          :header-line-width 4
;;          :mode-line-width 10
;;          :tab-width 4
;;          :right-divider-width 30
;;          :scroll-bar-width 8
;;          :fringe-width 8)))

(use-package denote-search
  :load-path "packages/denote-search/"
  :commands denote-search)


(provide 'init-note)
;;; init-note.el ends here.
