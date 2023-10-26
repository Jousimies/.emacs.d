;; init-note.el.el --- Note taking. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package ekg
  :load-path "packages/ekg/" "packages/triples/" "packages/llm"
  :commands (ekg-show-notes-in-trash
             ekg-show-notes-for-today
             ekg-show-notes-with-tag
             ekg-show-notes-with-all-tags
             ekg-show-notes-with-any-tags
             ekg-show-rename-tag
             ekg-browse-url)
  :bind (("C-<f10>" . ekg-capture)
         ("M-<f10>" . ekg-capture-url)
         ("C-c n t" . ekg-show-notes-for-today)
         (:map ekg-notes-mode-map
               ("q" . quit-window)))
  :config
  (setq triples-default-database-filename
        (expand-file-name "database/triples.db" my-galaxy)))

(use-package denote
  :load-path "packages/denote/"
  :bind (("s-n s" . denote-signature)
         ("s-n S" . denote-subdirectory)
         ("s-l l" . denote-link)
         ("s-l L" . denote-link-insert-links-matching-regexp)
         ("s-n r" . denote-rename-file-using-front-matter)
         ("s-n k" . denote-keywords-add)
         ("s-n K" . denote-keywords-remove)
         (:map dired-mode-map
               ("r" . denote-dired-rename-marked-files)))
  :hook ((dired-mode . denote-dired-mode-in-directories)
         (org-mode . (lambda ()
                       (require 'denote))))
  :config
  (setq denote-directory (expand-file-name "denote" my-galaxy))
  (setq denote-file-name-letter-casing
        '((title . downcase)
          (signature . verbatim)
          (keywords . verbatim)
          (t . downcase)))
  (setq denote-dired-directories
        (list denote-directory
              (thread-last denote-directory (expand-file-name "books"))
              (thread-last denote-directory (expand-file-name "outline"))
              (thread-last denote-directory (expand-file-name "literature"))
              (thread-last denote-directory (expand-file-name "term"))
              (thread-last denote-directory (expand-file-name "references")))))

(use-package denote-org-dblock
  :commands denote-org-dblock-insert-backlinks denote-org-dblock-insert-links)

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

(with-eval-after-load 'org-capture
  (require 'denote)
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  (add-to-list 'org-capture-templates
               '("n" "New denote note" plain
                 (file denote-last-path)
                 (function
                  (lambda ()
                    (denote-org-capture-with-prompts :title :keywords :subdirectory)))
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

(use-package denote-rename-buffer
  :load-path "packages/denote/"
  :hook (org-mode . denote-rename-buffer-mode)
  :custom
  (denote-rename-buffer-format " %t"))

(defvar prot-dired--limit-hist '()
  "Minibuffer history for `prot-dired-limit-regexp'.")

(defun my/dired-denote-signature-get ()
  (let* ((file (dired-get-filename))
         (signature (denote-retrieve-filename-signature file)))
    (concat "==" signature )))

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
         (title (cadr (split-string (cadr (grab-mac-link-safari-1)) "— ")))
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
    (when xwidget?
      (if (not (file-exists-p file-path))
          (when (my/save-xwidget-to-webarchive)
            (my/literature-entry url title keywords file-path file-new-path))
        (my/literature-entry url title keywords file-path file-new-path)))))

(defun my/literature-save ()
  "Save a literature entry from either Safari or xwidget-webkit."
  (interactive)
  (if (string-prefix-p "xwidget" (buffer-name))
      (my/literature-save-from-xwidget)
    (my/literature-save-from-safari)))

(global-set-key (kbd "s-s") 'my/literature-save)

(defconst ANNOTATION-DB '("~/Library/Containers/com.apple.iBooksX/Data/Documents/AEAnnotation/AEAnnotation_v10312011_1727_local.sqlite"))
(defconst BOOK-DB '("~/Library/Containers/com.apple.iBooksX/Data/Documents/BKLibrary/BKLibrary-1-091020131601.sqlite"))

(defconst APPLE-EPOCH-START (float-time (encode-time 0 0 0 1 1 2001)))

(defconst SELECT-ALL-ANNOTATIONS-QUERY
  "SELECT ZANNOTATIONASSETID AS assetId,
          ZANNOTATIONSELECTEDTEXT AS quote,
          ZANNOTATIONNOTE AS comment,
          ZFUTUREPROOFING5 AS chapter,
          ZANNOTATIONSTYLE AS colorCode,
          ZANNOTATIONMODIFICATIONDATE AS modifiedAt,
          ZANNOTATIONCREATIONDATE AS createdAt
   FROM ZAEANNOTATION
   WHERE ZANNOTATIONDELETED = 0
     AND ZANNOTATIONSELECTEDTEXT IS NOT NULL
     AND ZANNOTATIONSELECTEDTEXT <> ''
   ORDER BY ZANNOTATIONASSETID, ZPLLOCATIONRANGESTART;")

(defconst SELECT-ALL-BOOKS-QUERY
  "SELECT ZASSETID AS id, ZTITLE AS title, ZAUTHOR AS author
   FROM ZBKLIBRARYASSET")

(defun convert-apple-time (apple-time)
  (float-time (encode-time 0 0 0 1 1 2001)))

(defun fn/create-db (filename)
  (sqlite-open filename))

(defun fn/get-books-from-db (filename)
  (let ((db (fn/create-db filename)))
    (sqlite-execute db SELECT-ALL-BOOKS-QUERY)))

(defun fn/get-books ()
  (let ((books (mapcar 'fn/get-books-from-db BOOK-DB)))
    (apply 'append books)))

(defun fn/get-annotations-from-db (filename)
  (let ((db (fn/create-db filename)))
    (sqlite-execute db SELECT-ALL-ANNOTATIONS-QUERY)))

(defun fn/get-annotations ()
  (let ((annotations (mapcar 'fn/get-annotations-from-db ANNOTATION-DB)))
    (apply 'append annotations)))

(defun write-book-headings (books org-file)
  (with-temp-buffer
    (insert-file-contents org-file)
    (goto-char (point-max))
    (dolist (book books)
      (insert (format "* %s\n" (cadr book))))
    (write-file org-file)))

(defun fn/get-annotations-for-book (book-id)
  (cl-remove-if-not (lambda (annotation)
                      (string= (car annotation) book-id))
                    (fn/get-annotations)))

(defun fn/get-annotations-count (book-id)
  "Get the number of annotations for a book with BOOK-ID."
  (length (fn/get-annotations-for-book book-id)))

(defun fn/write-annotations-to-file (annotations note-path)
  "Write ANNOTATIONS to the specified NOTE-PATH."
  (with-temp-buffer
    (dolist (annot annotations)
      (insert (format "- %s\n" (cadr annot))))
    (append-to-file (point-min) (point-max) note-path)))

(defun fn/choose-book-and-save-to-file (note-path)
  "Choose a book and save its annotations to NOTE-PATH."
  (interactive "FOrg file to store annotations: ")
  (let* ((books (fn/get-books))
         (book-alist (mapcar (lambda (book)
                              (let* ((book-id (car book))
                                     (book-title (cadr book))
                                     (annot-count (fn/get-annotations-count book-id)))
                                (cons (format "[%d] %s" annot-count book-title) book-id)))
                            books))
         (selected-book-id (completing-read "Choose a book: " book-alist))
         (selected-book (cdr (assoc selected-book-id book-alist)))
         (annotations (fn/get-annotations-for-book selected-book))
         (annot-num (length annotations)))
    (when (not (string= selected-book-id ""))
      (if annotations
          (progn
            (fn/write-annotations-to-file annotations note-path)
            (message "%d annotations for %s written to %s" annot-num selected-book note-path))
        (message "No annotations in this book: %s." selected-book)))))

(use-package denote-journal-extras
  :load-path "~/.emacs.d/packages/denote/"
  :bind ("C-c f j" . denote-journal-extras-new-or-existing-entry)
  :commands denote-journal-extras--entry-today)

(use-package denote-menu
  :load-path "packages/denote-menu/"
  :bind ("C-c f m" . denote-menu-list-notes)
  :config
  (define-key denote-menu-mode-map (kbd "c") #'denote-menu-clear-filters)
  (define-key denote-menu-mode-map (kbd "/ r") #'denote-menu-filter)
  (define-key denote-menu-mode-map (kbd "/ k") #'denote-menu-filter-by-keyword)
  (define-key denote-menu-mode-map (kbd "/ o") #'denote-menu-filter-out-keyword)
  (define-key denote-menu-mode-map (kbd "/ s") #'my/denote-menu-filter-by-signature)
  (define-key denote-menu-mode-map (kbd "e") #'denote-menu-export-to-dired)
  (setq denote-menu-show-file-signature t)
  (setq-local tabulated-list-sort-key "Signature")
  (defun my/denote-menu-filter-by-signature ()
    (interactive)
    (setq denote-menu-current-regex "==\\([0-9][a-z]\\)*")
    (denote-menu-update-entries)
    (tabulated-list-sort 1)))

(use-package consult-notes
  :load-path "packages/consult-notes/"
  :bind ("s-N" . consult-notes)
  :config
  (setq consult-notes-file-dir-sources
        `(("Articles"  ?a  ,(concat my-galaxy "/blogs_source/posts"))
          ("Denote Notes"  ?d ,(expand-file-name "denote" my-galaxy))
          ("Terminology"  ?t ,(expand-file-name "denote/term" my-galaxy))
          ("Book Reading"  ?b ,(expand-file-name "denote/books" my-galaxy))
          ("Outline"  ?o ,(expand-file-name "denote/outline" my-galaxy))
          ("Meet"  ?m ,(expand-file-name "meeting" my-galaxy))
          ("References"  ?r ,(expand-file-name "denote/references" my-galaxy))
          ("Literature"  ?l ,(expand-file-name "denote/literature" my-galaxy))
          ("Journal"  ?j ,(expand-file-name "denote/journal" my-galaxy))
          ("Logs"  ?L ,(expand-file-name "logs" my-galaxy))
          )))

(defun my/new-article (article)
  (interactive "sTitle: ")
  (let ((filename (format "%s" article))
        (ext ".org"))
    (find-file (concat website-directory "posts/" filename ext))
    (insert "#+TITLE: " article "\n")
    (tempel-insert 'blog)))
(global-set-key (kbd "C-c n a") 'my/new-article)

(defun my/new-meeting (meet)
  (interactive "sTitle: ")
  (let ((filename (format "%s-%s" (format-time-string "%Y%m%d") meet))
        (ext ".org"))
    (find-file (concat my-galaxy "/meeting/" filename ext))
    (insert "#+TITLE: " meet "\n")
    (tempel-insert 'meeting)))

(global-set-key (kbd "C-c n m") 'my/new-meeting)

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

(with-eval-after-load 'org
  (add-to-list 'org-options-keywords "AUTO_EXPORT:"))

(defun auto-export-blog ()
  "Auto export blog."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char 0)
      (if (string-equal (car
                         (cdr
                          (car
                           (org-collect-keywords '("AUTO_EXPORT")))))
                        "t")
          (org-publish-all)))))

(add-hook 'after-save-hook 'auto-export-blog)

(provide 'init-note)
;;; init-note.el ends here.
