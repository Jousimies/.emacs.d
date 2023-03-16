(use-package undo-fu
  :defer t)

(use-package undo-fu-session
  :after undo-fu
  :hook (on-first-file . undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-directory (expand-file-name "cache/undo-fu-session" user-emacs-directory)))

(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  :bind ("C-x u" . vundo))

(provide 'init-undo)
;;; init-undo.el ends here.
