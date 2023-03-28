(define-fringe-bitmap 'right-curly-arrow  [])
(define-fringe-bitmap 'left-curly-arrow  [])

(fringe-mode '(1 . 1))

(use-package hl-line
  :hook ((prog-mode . hl-line-mode)
         (org-mode . hl-line-mode)
         (profiler-report-mode . hl-line-mode)))

(use-package display-line-numbers
  :hook ((prog-mode . display-line-numbers-mode)
         (org-mode . display-line-numbers-mode)
         (LaTeX-mode . display-line-numbers-mode))
  :init
  (setq-default display-line-numbers-widen t)
  :config
  (setq display-line-numbers-type 'relative))

(scroll-bar-mode 0)

(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode)
  :config
  (face-spec-set 'fill-column-indicator
                 '((default :height 0.1))
                 'face-override-spec)
  (setq-default fill-column 90))

(use-package paren
  :hook (text-mode . show-paren-mode)
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-context-when-offscreen 'overlay))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(evil-paste-after
                   evil-paste-pop
                   evil-paste-before
                   evil-delete
                   evil-delete-line))
  (advice-add command :after #'pulse-line))

(use-package color-identifiers-mode
  :hook (text-mode . global-color-identifiers-mode))

(use-package page-break-lines
  :hook (org-mode . page-break-lines-mode))

(provide 'init-ui)
;;; init-ui.el ends here.
