;;; init-org+.el --- Enhance org.   -*- lexical-binding: t no-byte-compile: t -*-
;;; Code:
;;; Commentary:

;; Zen
(when (maybe-require-package 'writeroom-mode))

;; Table of content of org files.
(when (maybe-require-package 'toc-org)
  (add-hook 'org-mode-hook 'toc-org-mode))

;; Star.
(when (maybe-require-package 'org-superstar)
  (add-hook 'org-mode-hook 'org-superstar-mode))

;; Attachment and download.
(when (maybe-require-package 'org-download)
  (add-hook 'org-mode-hook 'org-download-enable)
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
  (setq math-preview-margin '(1 . 0)))

;; Plantuml
(add-hook 'org-mode-hook (lambda ()
                           (require 'plantuml)))
(with-eval-after-load 'plantuml
  (setq plantuml-jar-path
        (concat
         (string-trim
          (shell-command-to-string "readlink -f $(brew --prefix plantuml)"))
         "/libexec/plantuml.jar")))


(when (maybe-require-package 'org-rainbow-tags)
  (add-hook 'org-mode-hook 'org-rainbow-tags-mode))

(require-package 'boxy-headings)

;; (with-eval-after-load 'org
;;   (when (maybe-require-package 'idle-org-agenda)
;;     (add-hook 'org-mode-hook 'idle-org-agenda-mode)))

(require-package 'alarm-clock)
(with-eval-after-load 'alarm-clock
  (setq alarm-clock-cache-file (expand-file-name "var/.alarm-clock.cache" user-emacs-directory)))

(with-eval-after-load 'company
  (require-package 'company-org-block)
  (add-hook 'org-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           'company-org-block))))

(when (maybe-require-package 'org-alert)
  (run-with-idle-timer 20 nil (lambda ()
                               (require 'org-alert)))
  (with-eval-after-load 'org-alert
    (setq org-alert-interval 300)
    (setq org-alert-notify-cutoff 10)
    (setq org-alert-notify-after-event-cutoff 10)
    (setq org-alert-notification-title "Org Agenda Reminder!")
    (org-alert-enable)))

(when (maybe-require-package 'pomm)
  (with-eval-after-load 'pomm
    (setq pomm-state-file-location (expand-file-name "pomm" no-littering-var-directory))
    (pomm-mode-line-mode 1)))

(with-eval-after-load 'alert
  (setq alert-default-style 'notifier))


(provide 'init-org+)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org+.el ends here
