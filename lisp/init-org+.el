;; init-org+.el --- Better org with third packages. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package olivetti
  :bind ("C-c t o" . olivetti-mode))

(use-package org-appear
  :config
  (setq org-appear-autolinks t)
  (setq org-appear-trigger 'manual)
  :hook ((org-mode . (lambda ()
                       (add-hook 'meow-insert-enter-hook
                                 #'org-appear-manual-start
                                 nil
                                 t)
                       (add-hook 'meow-insert-exit-hook
                                 #'org-appear-manual-stop
                                 nil
                                 t)))
         (org-mode . org-appear-mode)))

(use-package math-preview
  :commands math-preview-all math-preview-clear-all
  :hook (org-mode . auto/math-preview-all)
  :config
  (setq math-preview-scale 1.1)
  (setq math-preview-raise 0.2)
  (setq math-preview-margin '(1 . 0))
  (add-to-list 'org-options-keywords "NO_MATH_PREVIEW:")

  (defun auto/math-preview-all ()
    "Auto update clock table."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char 0)
        (unless (string-equal (cadar (org-collect-keywords '("NO_MATH_PREVIEW"))) "t")
          (when (re-search-forward "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}" (point-max) t)
              (math-preview-all)))))))

(use-package org-download
  :bind (("C-c d c" . org-download-clipboard)
         ("C-c d y" . org-download-yank)
         ("C-c d s" . org-download-screenshot)
         ("C-c d r" . org-download-rename-at-point))
  :init
  (setq org-download-image-dir (expand-file-name "pictures" my-galaxy))
  (setq org-download-heading-lvl nil)
  :config
  (setq org-download-screenshot-method "screencapture -i %s")
  (setq org-download-abbreviate-filename-function 'expand-file-name)
  (setq org-download-timestamp "%Y%m%d%H%M%S")
  (setq org-download-display-inline-images nil)
  (setq org-download-annotate-function (lambda (_link) ""))
  (setq org-download-image-attr-list '("#+NAME: fig: "
                                       "#+CAPTION: "
                                       "#+ATTR_ORG: :width 500px"
                                       "#+ATTR_LATEX: :width 10cm :placement [!htpb]"
                                       "#+ATTR_HTML: :width 600px"))

  (defun my/org-download-rename (arg)
    (interactive "P")
    (if arg
        (org-download-rename-last-file)
      (org-download-rename-at-point)))

  (defun my/auto-change-file-paths (&optional basename)
    (interactive)
    (save-excursion
      (previous-line)
      (while (re-search-forward (expand-file-name "~") nil t)
        (replace-match "~" t nil))))
  (advice-add 'org-download-clipboard :after 'my/auto-change-file-paths)
  (advice-add 'org-download-screenshot :after 'my/auto-change-file-paths))

(use-package org-imgtog
  :hook (org-mode . org-imgtog-mode))

(use-package plantuml
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

(defun add-symbol-to-region (beg end symbol)
  (save-excursion
    (goto-char end)
    (insert (concat symbol " "))
    (goto-char beg)
    (insert (concat " " symbol))))

(defun add-stars-to-region (beg end)
  (interactive "r")
  (add-symbol-to-region beg end "*"))

(defun add-equal-to-region (beg end)
  (interactive "r")
  (add-symbol-to-region beg end "="))

(defun add-underline-to-region (beg end)
  (interactive "r")
  (add-symbol-to-region beg end "_"))

(defun add-italic-to-region (beg end)
  (interactive "r")
  (add-symbol-to-region beg end "/"))

(defun add-plus-to-region (beg end)
  (interactive "r")
  (add-symbol-to-region beg end "+"))

;;;###autoload
(transient-define-prefix my/add-symbol-to-region ()
  "Add symbol."
  ["Commands"
   ("*" "star" add-stars-to-region)
   ("=" "equal" add-equal-to-region)
   ("_" "underline" add-underline-to-region)
   ("+" "plus" add-plus-to-region)
   ("/" "italic" add-italic-to-region)
   ]
  [("q" "Quit"           transient-quit-one)])

(use-package org-anki
  :commands org-anki-sync-entry org-anki-sync-all org-anki-delete-entry)

(provide 'init-org+)
;;; init-org+.el ends here.
