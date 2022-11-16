;;; init-org+.el --- Enhance org. 	-*- lexical-binding: t no-byte-compile: t -*-
;;; Code:
;;; Commentary:

;; Zen
(when (maybe-require-package 'writeroom-mode)

  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "tz" '(writeroom-mode :wk "Zen mode")))

;; Table of content of org files.
(when (maybe-require-package 'toc-org)
  (add-hook 'org-mode-hook 'toc-org-mode))

;; Star.
(when (maybe-require-package 'org-superstar)
  (add-hook 'org-mode-hook 'org-superstar-mode))

;; Attachment and download.
(when (maybe-require-package 'org-download)
  (add-hook 'org-mode-hook 'org-download-enable)
  (with-eval-after-load 'org-download
    (setq org-download-image-dir (expand-file-name "pictures" my-galaxy))
    (setq org-download-screenshot-method 'screencapture)
    (setq org-download-abbreviate-filename-function 'expand-file-name)
    (setq org-download-timestamp "%Y%m%d%H%M%S")
    (setq org-download-display-inline-images nil)
    (setq org-download-heading-lvl nil)
    (setq org-download-annotate-function (lambda (_link) ""))
    (setq org-download-image-attr-list '("#+NAME: fig: "
                                         "#+CAPTION: "
                                         "#+ATTR_ORG: :width 500px"
                                         "#+ATTR_LATEX: :width 10cm :placement [!htpb]"
                                         "#+ATTR_HTML: :width 600px"))
    (setq org-download-screenshot-basename ".png"))

  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "od" '(:ignore t :wk "Download")
   "odc" '(org-download-clipboard :wk "Download Clipboard")
   "ody" '(org-download-yank :wk "Download Yank")
   "odr" '(org-download-rename-last-file :wk "Rename last file")
   "odR" '(org-download-rename-at-point :wk "Rename point")))

;; Appear link
(when (maybe-require-package 'org-appear)
  (setq org-appear-trigger 'manual)
  (setq org-appear-autolinks t)

  (add-hook 'org-mode-hook 'org-appear-mode))
;; Link beautify
;; (when (maybe-require-package 'org-link-beautify)
;;   (add-hook 'org-mode-hook 'org-link-beautify-mode))

;; Math preview.
(when (maybe-require-package 'math-preview)
  (setq math-preview-scale 1.1)
  (setq math-preview-raise 0.3)
  (setq math-preview-margin '(1 . 0))

  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "tm" '(math-preview-all :wk "Math preveiw")))


;; Plantuml
(add-hook 'org-mode-hook (lambda ()
                           (require 'plantuml)))
(with-eval-after-load 'plantuml
  (setq plantuml-jar-path
        (concat
         (string-trim
          (shell-command-to-string "readlink -f $(brew --prefix plantuml)"))
         "/libexec/plantuml.jar")))
(general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "op" '(:ignore t :wk "Plantuml")
   "opm" '(plantuml-org-to-mindmap-open :wk "Mindmap")
   "ops" '(plantuml-org-to-wbs-open :wk "Work Breakdown Structure"))

(provide 'init-org+)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org+.el ends here
