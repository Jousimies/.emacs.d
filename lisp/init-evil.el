;;; init-evil.el --- Evil. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(when (require-package 'evil)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  (add-hook 'on-first-buffer-hook 'evil-mode)

  (when (maybe-require-package 'evil-commentary)
    (add-hook 'on-first-input-hook 'evil-commentary-mode))

  (when (maybe-require-package 'evil-collection)
    (with-eval-after-load 'evil
      (evil-collection-init)))
    ;; (add-hook 'after-init-hook 'evil-collection-init))

  (when (maybe-require-package 'evil-surround)
    (add-hook 'on-first-input-hook 'global-evil-surround-mode))

  (require-package 'evil-anzu)

  (when (maybe-require-package 'evil-embrace)
    (add-hook 'org-mode-hook 'embrace-org-mode-hook)
    (with-eval-after-load 'evil
      (evil-embrace-enable-evil-surround-integration))))

(require-package 'general)

(when (require-package 'which-key)
  (which-key-mode 1)
  (with-eval-after-load 'which-key
    (setq which-key-idle-delay 0.3)))

(provide 'init-evil)
;;; init-evil.el ends here