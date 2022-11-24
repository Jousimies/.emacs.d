;;; init-tempel.el --- Template.  	-*- lexical-binding: t no-byte-compile: t -*-
;;; Code:
(when (maybe-require-package 'tempel)
  (setq tempel-path `("~/.emacs.d/template/tempel"
                      ,(expand-file-name "template/tempel" my-galaxy)))

  (global-set-key (kbd "M-+") 'tempel-complete)
  (global-set-key (kbd "M-*") 'tempel-insert))

(when (maybe-require-package 'yasnippet)
  (add-hook 'after-init-hook 'yas-global-mode)

  (require-package 'yasnippet-snippets))

(provide 'init-tempel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tempel.el ends here
