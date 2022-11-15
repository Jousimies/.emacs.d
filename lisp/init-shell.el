;;; init-shell.el --- Shell.  -*- lexical-binding: t no-byte-compile: t -*-
;;; Code:
(when (maybe-require-package 'vterm)
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000)

  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "tv" '(vterm :wk "vterm")))

(provide 'init-shell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
