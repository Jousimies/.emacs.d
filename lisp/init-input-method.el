;; init-input-method.el --- Input methods. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;; Earlier, I use emacs-rime. Now, sis I selected.

;;; Code:

(use-package rime
    :defer t
    :hook ((meow-insert-enter . (lambda ()
                                  (if (and (not (rime--should-inline-ascii-p))
                                           (eq major-mode 'org-mode)
                                           (not (org-at-clock-log-p))
                                           (not (org-at-table-p))
                                           (not (org-at-timestamp-p))
                                           (not (and (bolp) (org-on-heading-p))))
                                      (progn
                                        (activate-input-method "rime")
                                        (im-change-cursor-color)))))
           (meow-insert-exit .  (lambda ()
                                  (deactivate-input-method)
                                  (set-cursor-color im-default-cursor-color))))
    :init
    (setq rime-title " ")
    :config
    (defvar im-cursor-color "red"
      "The color for input method.")

    (defvar im-default-cursor-color (frame-parameter nil 'cursor-color)
      "The default cursor color.")

    (defun im--chinese-p ()
      "Check if the current input state is Chinese."
      (if (featurep 'rime)
          (and (rime--should-enable-p)
               (not (rime--should-inline-ascii-p))
               current-input-method)
        current-input-method))

    (defun im-change-cursor-color ()
      "Set cursor color depending on input method."
      (interactive)
      (set-cursor-color (if (im--chinese-p)
                            im-cursor-color
                          im-default-cursor-color)))
    (setq default-input-method "rime")
    (setq rime-user-data-dir "~/Library/Rime/")
    (setq rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include/")
    (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
    (setq rime-disable-predicates '(rime-predicate-prog-in-code-p
                                    rime-predicate-org-in-src-block-p
                                    rime-predicate-org-latex-mode-p
                                    rime-predicate-tex-math-or-command-p))
    (setq rime-inline-predicates '(rime-predicate-space-after-cc-p
                                   rime-predicate-after-alphabet-char-p)))

(use-package rime-regexp
  :hook (after-init . rime-regexp-mode))

(provide 'init-input-method)
;;; init-input-method.el ends here.
