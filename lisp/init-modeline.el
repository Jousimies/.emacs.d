;;; init-modeline.el --- Modeline.	-*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;
;;; Code:

(when (require-package 'doom-modeline)
  (add-hook 'after-init-hook 'doom-modeline-mode)
    (setq doom-modeline-icon t)
    (setq doom-modeline-height 20))

(require-package 'mode-line-bell)
(add-hook 'after-init-hook 'mode-line-bell-mode)

(provide 'init-modeline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-modeline.el ends here
