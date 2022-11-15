;;; init-rime.el --- Input method   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(when (maybe-require-package 'rime)
  (setq rime-user-data-dir "~/Library/Rime/")
  (setq rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-plus@28/28.2/include")
  (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
  (setq default-input-method "rime")
  ;; (setq rime-title `(,(propertize (all-the-icons-faicon "pencil-square-o" :v-adjust -0.1)
;;                                'face `(:family ,(all-the-icons-faicon-family)))))
  (setq rime-show-candidate 'minibuffer)
  (setq rime-posframe-properties '(:internal-border-width 0))
  (setq rime-disable-predicates '(rime-predicate-prog-in-code-p
				                  rime-predicate-org-in-src-block-p
				                  rime-predicate-org-latex-mode-p
				                  rime-predicate-current-uppercase-letter-p))

  (setq rime-inline-predicates '(rime-predicate-space-after-cc-p
				                 rime-predicate-after-alphabet-char-p))

  (with-eval-after-load 'rime
    (define-key rime-mode-map (kbd "M-j") 'rime-force-enable))

  (with-eval-after-load 'evil
    (add-hook 'evil-insert-state-entry-hook (lambda ()
					                          (if (eq major-mode 'org-mode)
						                          (activate-input-method "rime"))))
    (add-hook 'evil-insert-state-exit-hook #'evil-deactivate-input-method)))

(when (require 'rime-regexp)
  (with-eval-after-load 'rime
    (rime-regexp-mode t)))


(provide 'init-rime)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rime.el ends here
