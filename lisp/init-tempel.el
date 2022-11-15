;;; init-tempel.el --- Template.  	-*- lexical-binding: t no-byte-compile: t -*-
;;; Code:
(when (maybe-require-package 'tempel)
  (setq tempel-path "~/.emacs.d/template/tempel")

  (global-set-key (kbd "M-+") 'tempel-complete)
  (global-set-key (kbd "M-*") 'tempel-insert))

(provide 'init-tempel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tempel.el ends here
