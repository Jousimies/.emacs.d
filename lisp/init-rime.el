(use-package sis
  :config
  (setq sis-other-cursor-color "red")
  (setq sis-english-source "com.apple.keylayout.ABC")
  (setq sis-other-source "im.rime.inputmethod.Squirrel.Hans")
  (add-hook 'evil-insert-state-exit-hook #'sis-set-english)
  (sis-global-cursor-color-mode t)
  (sis-global-context-mode t)
  (sis-global-respect-mode t)
  (sis-global-inline-mode t))

(provide 'init-rime)
;;; init-rime.el ends here.
