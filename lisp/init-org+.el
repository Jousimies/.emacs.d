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
  :general (my/space-leader-def
             "p" '(:ignore t :wk "Preview")
             "pa" '(math-preview-all :wk "All")
             "pA" '(math-preview-clear-all :wk "Clear All")
             "pp" '(math-preview-at-point :wk "Point")
             "pP" '(math-preview-clear-at-point :wk "Clear Point")
             "pr" '(math-preview-region :wk "Region")
             "pR" '(math-preview-clear-region :wk "Clear Region"))
  :config
  (setq math-preview-scale 1.1)
  (setq math-preview-raise 0.3)
  (setq math-preview-margin '(1 . 0)))

(use-package org-download
  :hook (org-mode . org-download-enable)
  :general (my/space-leader-def
             "d" '(:ignore t :wk "Download")
             "dc" '(org-download-clipboard :wk "Download Clipboard")
             "dy" '(org-download-yank :wk "Download Yank")
             "dr" '(org-download-rename-last-file :wk "Rename last file")
             "dR" '(org-download-rename-at-point :wk "Rename point"))
  :init
  (setq org-download-image-dir (expand-file-name "pictures" my-galaxy))
  (setq org-download-heading-lvl nil)
  :config
  (setq org-download-screenshot-method 'screencapture)
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
  (advice-add 'org-download-clipboard :after 'my/auto-change-file-paths))

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
