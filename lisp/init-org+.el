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

(defun add-symbol-to-region (beg end symbol)
  (save-excursion
    (goto-char end)
    (insert (concat symbol " "))
    (goto-char beg)
    (insert (concat " " symbol))))

(defun add-stars-to-region (beg end)
  (interactive "r")
  (add-symbol-to-region beg end "*"))

(defun add-verbatim-to-region (beg end)
  (interactive "r")
  (add-symbol-to-region beg end "~"))

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
   ("~" "verbatim" add-verbatim-to-region)
   ("_" "underline" add-underline-to-region)
   ("+" "plus" add-plus-to-region)
   ("/" "italic" add-italic-to-region)
   ]
  [("q" "Quit"           transient-quit-one)])

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

(provide 'init-org+)
;;; init-org+.el ends here.
