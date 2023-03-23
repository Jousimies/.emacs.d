(use-package sis
  :config
  (setq sis-other-cursor-color "red")
  (setq sis-external-ism "im-select")
  (sis-ism-lazyman-config "com.apple.keylayout.ABC" "im.rime.inputmethod.Squirrel.Hans")
  (sis-global-cursor-color-mode t)
  (sis-global-context-mode t)
  (sis-global-respect-mode t)
  (sis-global-inline-mode t)
  (add-to-list 'sis-context-detectors
               (lambda (&rest _)
                 ;; (when (or (and (eq major-mode 'org-mode) (org-at-heading-p))
                 (when (or (eq major-mode 'org-mode)
                           (eq major-mode 'telega-chat-mode))
                   'other))))

(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (setq sis-default-cursor-color (foreground-color-at-point))))

(provide 'init-sis)
;;; init-sis.el ends here.
