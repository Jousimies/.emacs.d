;;; init-evil.el --- Evil. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(when (require-package 'evil)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  (evil-mode 1)

  (when (maybe-require-package 'evil-commentary)
    (evil-commentary-mode 1))

  (when (maybe-require-package 'evil-collection)
    (add-hook 'after-init-hook 'evil-collection-init))

  (when (maybe-require-package 'evil-surround)
    (global-evil-surround-mode 1)))

(require-package 'general)

(when (require-package 'which-key)
  (which-key-mode 1)
  (with-eval-after-load 'which-key
    (setq which-key-idle-delay 0.3)))

(provide 'init-evil)
;;; init-evil.el ends here
