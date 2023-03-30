;; init-org+.el --- Better org with third packages. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package olivetti
  :commands olivetti-mode)

(evil-define-key 'normal org-mode-map
    "zw" 'olivetti-mode)

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("❶" "❷" "❸" "❹" "❺" "❻" "❼"))
  ;; (setq org-superstar-headline-bullets-list '("1" "2" "3" "4" "5" "6" "7"))
  ;; (setq org-superstar-headline-bullets-list '("①" "②" "③" "④" "⑤" "⑥"))
  (setq org-hide-leading-stars t))

(use-package org-rainbow-tags
  :hook (org-mode . org-rainbow-tags-mode))

(use-package org-appear
  :config
  (setq org-appear-autolinks t)
  (setq org-appear-trigger 'manual)
  :hook ((org-mode . (lambda ()
                       (add-hook 'evil-insert-state-entry-hook
                                 #'org-appear-manual-start
                                 nil
                                 t)
                       (add-hook 'evil-insert-state-exit-hook
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

  (defun my/org-download-rename (arg)
    (interactive "P")
    (if arg
        (org-download-rename-last-file)
      (org-download-rename-at-point)))

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
  :hook (org-mode . org-download-enable)
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
  (defun my/auto-change-file-paths ()
    (interactive)
    (save-excursion
      (previous-line)
      (while (re-search-forward (expand-file-name "~") nil t)
        (replace-match "~" t nil))))
  (advice-add 'org-download-clipboard :after 'my/auto-change-file-paths)
  (advice-add 'org-download-screenshot :after 'my/auto-change-file-paths))

(use-package plantuml
  :general (my/space-leader-def
             "pm" '(plantuml-org-to-mindmap-open :wk "Mindmap")
             "ps" '(plantuml-org-to-wbs-open :wk "Work Breakdown Structure"))
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

(use-package org-anki
  :commands org-anki-sync-entry org-anki-sync-all org-anki-delete-entry)

(use-package org-contacts
  :commands org-contacts
  :config
  (setq org-contacts-files `(,(expand-file-name "people/contacts.org" my-galaxy))))

(provide 'init-org+)
;;; init-org+.el ends here.
