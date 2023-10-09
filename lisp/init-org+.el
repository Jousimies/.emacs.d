;; init-org+.el --- Better org with third packages. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package olivetti
  :load-path "packages/olivetti/"
  :bind ("s-M-z" . olivetti-mode))

(use-package math-preview
  :load-path "packages/math-preview/"
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
  :load-path "packages/org-download/"
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

  ;; (defun my/auto-change-file-paths (&optional basename)
  ;;   (interactive)
  ;;   (save-excursion
  ;;     (previous-line)
  ;;     (while (re-search-forward (expand-file-name "~") nil t)
  ;;       (replace-match "~" t nil))))
  ;; (advice-add 'org-download-clipboard :after 'my/auto-change-file-paths)
  ;; (advice-add 'org-download-screenshot :after 'my/auto-change-file-paths)

  (advice-add 'org-download-clipboard :after 'my/org-download-adjust)
  ;; (advice-add 'org-download-screenshot :after 'my/org-download-adjust)

  (defun my/org-download-adjust (&optional basename)
    "Adjust the last downloaded file.

  This function renames the last downloaded file, replaces all occurrences of the old file name with the new file name in the Org mode buffer, and updates the CAPTION and NAME headers in the Org mode buffer. "
    (interactive)
    (let* ((dir-path (org-download--dir))
           (newname (read-string "Rename last file to: " (file-name-base org-download-path-last-file)))
           (ext (file-name-extension org-download-path-last-file))
           (newpath (concat dir-path "/" newname "." ext)))
      (when org-download-path-last-file
        (rename-file org-download-path-last-file newpath 1)
        (org-download-replace-all
         (file-name-nondirectory org-download-path-last-file)
         (concat newname "." ext))
        (setq org-download-path-last-file newpath))
      (save-excursion
        (previous-line 7)
        (while (re-search-forward "^\\#\\+NAME: fig:" nil t 1)
          (move-end-of-line 1)
          (insert newname))
        (while (re-search-forward "^\\#\\+CAPTION:" nil t 1)
          (move-end-of-line 1)
          (insert newname))
        (while (re-search-forward (expand-file-name "~") nil t 1)
          (replace-match "~" t nil))))))

(use-package org-imgtog
  :load-path "packages/org-imgtog/"
  :hook (org-mode . org-imgtog-mode))

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

(defun my/add-symbol-to-region (beg end symbol)
  "Add a SYMBOL to the BEG and END of region."
  (interactive "r\nsEnter symbol(+_~=*/): ")
  (save-excursion
    (goto-char end)
    (insert symbol)
    (unless (looking-at " ")
      (insert " "))
    (goto-char beg)
    (unless (or (bolp) (looking-back " "))
      (insert " "))
    (insert symbol)))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "s-b") 'my/add-symbol-to-region))

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

(use-package hammy
  :load-path "packages/hammy.el/" "packages/svg-lib" "packages/ts.el"
  :commands hammy-start
  :bind (("<f9>" . hammy-start)
         ("C-<f9>" . hammy-stop))
  :hook (hammy-start . hammy-mode)
  :custom
  (hammy-mode-always-show-lighter nil)
  (hammy-mode-lighter-prefix "")
  (hammy-mode-lighter-pie nil)
  :config
  ;; https://github.com/alphapapa/hammy.el/issues/10
  ;; Use alert instead of notifications-notify, which do not work on MacOS platform.
  (define-advice notifications-notify
      (:override (&rest params) using-alert)
    (alert (plist-get params :body)
           :title (plist-get params :title))))

(provide 'init-org+)
;;; init-org+.el ends here.
