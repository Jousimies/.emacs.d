;;; init-pomodoro.el --- Pomodoro. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(when (maybe-require-package 'pomm)
  (with-eval-after-load 'pomm
    (setq pomm-state-file-location (expand-file-name "pomm" no-littering-var-directory))
    (pomm-mode-line-mode 1)))

(with-eval-after-load 'alert
  (setq alert-default-style 'notifier))


(provide 'init-pomodoro)
;;; init-pomodoro.el ends here
