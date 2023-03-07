;; init-rime.el --- rime *- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package rime
  :init
  (setq rime-title "𝐑 ")
  (setq rime-user-data-dir "~/Library/Rime/")
  (setq rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include/")
  (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
  :config
  (setq default-input-method "rime")
  (setq rime-show-candidate 'minibuffer)
  ;; (setq rime-posframe-properties '(:internal-border-width 0))
  (setq rime-disable-predicates '(rime-predicate-prog-in-code-p
                                  rime-predicate-org-in-src-block-p
                                  rime-predicate-org-latex-mode-p
                                  rime-predicate-tex-math-or-command-p))

  (setq rime-inline-predicates '(rime-predicate-space-after-cc-p
                                 rime-predicate-after-alphabet-char-p))
  :bind (:map rime-mode-map
              ("M-j" . rime-force-enable))
  :hook ((evil-insert-state-entry . (lambda ()
                                      (if (and (not (rime--should-inline-ascii-p))
                                               (eq major-mode 'org-mode)
                                               (not (org-at-clock-log-p))
                                               (not (org-at-table-p))
                                               (not (org-at-timestamp-p))
                                               (not (and (bolp) (org-on-heading-p))))
                                          (activate-input-method "rime"))))
         (evil-insert-state-exit .  #'evil-deactivate-input-method)))

(use-package rime-regexp
  :hook (on-first-input . rime-regexp-mode))

(provide 'init-rime)
;;; init-rime.el ends here.
