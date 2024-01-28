;; init-note.el.el --- Note taking. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package denote
  :load-path "packages/denote/"
  :commands (denote-signature
			 denote-subdirectory
			 denote-org-capture-with-prompts
			 denote-link
			 denote-backlinks
			 denote-rename-file-using-front-matter
			 denote-keywords-add
			 denote-keywords-remove)
  :bind ((:map dired-mode-map
               ("r" . denote-dired-rename-marked-files-with-keywords)))
  :hook (dired-mode . denote-dired-mode-in-directories)
  :config
  (setq denote-rename-no-confirm t)
  (setq denote-directory (expand-file-name "denote" my-galaxy))
  (setq denote-file-name-slug-functions '((title . denote-sluggify-title)
										  (signature . denote-sluggify-signature)
										  (keyword . identity)))
  (setq denote-dired-directories
        (list denote-directory
              (thread-last denote-directory (expand-file-name "books"))
              (thread-last denote-directory (expand-file-name "outline"))
              (thread-last denote-directory (expand-file-name "literature"))
              (thread-last denote-directory (expand-file-name "term"))
              (thread-last denote-directory (expand-file-name "references")))))

(use-package denote-org-dblock
  :commands denote-org-dblock-insert-backlinks denote-org-dblock-insert-links)

(use-package denote-org-extras
  :commands denote-org-extras-extract-org-subtree denote-org-extras-link-to-heading)

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
(defun my/denote-signature-from-filename ()
  "Denotes the signature from the filename and kills it."
  (interactive)
  (let* ((mode (buffer-local-value 'major-mode (current-buffer)))
         (file (if (eq mode 'org-mode) (buffer-file-name) (dired-get-filename)))
         (signature (denote-retrieve-filename-signature file)))
    (if signature
        (kill-new signature))))

(advice-add 'denote-signature :before #'my/denote-signature-from-filename)

(use-package denote-rename-buffer
  :load-path "packages/denote/"
  :hook (org-mode . denote-rename-buffer-mode)
  :config
  (setq denote-rename-buffer-format " %t"))

(defvar prot-dired--limit-hist '()
  "Minibuffer history for `prot-dired-limit-regexp'.")

(defun my/dired-denote-signature-get ()
  (let* ((file (dired-get-filename))
         (signature (denote-retrieve-filename-signature file)))
    (concat "==" signature )))

;;;autoloads
(defun prot-dired-limit-regexp (regexp omit)
  "Limit Dired to keep files matching REGEXP, default search with Signature.

With optional OMIT argument as a prefix (\\[universal-argument]),
exclude files matching REGEXP.

Restore the buffer with \\<dired-mode-map>`\\[revert-buffer]'."
  (interactive
   (list
    (read-regexp
     (concat "Files "
             (when current-prefix-arg
               (propertize "NOT " 'face 'warning))
             "matching PATTERN" (format " (default: %s)" (my/dired-denote-signature-get)) ": ")
     (my/dired-denote-signature-get)
     nil)
    current-prefix-arg))
  (dired-mark-files-regexp regexp)
  (unless omit (dired-toggle-marks))
  (dired-do-kill-lines))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "/ r") 'prot-dired-limit-regexp))

(defun my/denote-org-extract-subtree-with-signature ()
  (interactive)
  (if-let ((text (org-get-entry))
           (heading (denote-link-ol-get-heading)))
      (let ((tags (org-get-tags))
            (date (denote-org-extras--get-heading-date)))
        (delete-region (org-entry-beginning-position)
                       (save-excursion (org-end-of-subtree t) (point)))
        (denote heading tags 'org nil date nil (denote-signature-prompt))
        (insert text))
    (user-error "No subtree to extract; aborting")))

(defun my/denote-org-extract-subtree-to-subdirectory ()
  (interactive)
  (if-let ((text (org-get-entry))
           (heading (denote-link-ol-get-heading)))
      (let ((tags (org-get-tags))
            (date (denote-org-extras--get-heading-date)))
        (delete-region (org-entry-beginning-position)
                       (save-excursion (org-end-of-subtree t) (point)))
        (denote heading tags 'org (denote-subdirectory-prompt) date)
        (insert text))
    (user-error "No subtree to extract; aborting")))

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
          ("Outline"  ?o ,(expand-file-name "denote/outline" my-galaxy))
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
  :config
  (defun my/denote-sort-with-sigature ()
	(interactive)
	(denote-sort-dired (denote-files-matching-regexp-prompt) 'signature nil))
  (defun my/denote-sort-with-identifer ()
	(interactive)
	(denote-sort-dired (denote-files-matching-regexp-prompt) 'identifier nil))
  (defun my/denote-sort-with-keywords ()
	(interactive)
	(denote-sort-dired (denote-files-matching-regexp-prompt) 'keywords nil)))

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
         (file-path (concat my/web_archive title ".html"))
         (file-new-path (concat my/web_archive new-title ".html")))
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
         (file-path (concat my/web_archive title ".html"))
         (file-new-path (concat my/web_archive new-title ".html")))
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
  :commands ibooks-annot/extract-annotations-to-note
  :config
  (setq pdfannots-script "~/.emacs.d/packages/pdfannots/pdfannots.py -f json")
  (setq ibooks-annot/book-note-directory (expand-file-name "denote/books" my-galaxy)))

(use-package denote-explore
  :load-path "packages/denote-explore/"
  :commands (denote-explore-count-notes
			 denote-explore-count-keywords
			 denote-explore-keywords-barchart
			 denote-explore-identify-duplicate-identifiers
			 denote-explore-rename-keyword))
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

(defun my/ocr ()
  "OCR with Macos system."
  (interactive)
  (shell-command "shortcuts run \"OCR Selected Area\"")
  (do-applescript "tell application id \"org.gnu.Emacs\" to activate"))

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
(provide 'init-note)
;;; init-note.el ends here.
