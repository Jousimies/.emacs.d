;; init-template.el --- Template. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :config
  (setq tempel-path `("~/.emacs.d/template/tempel"
                      ,(expand-file-name "config/tempel" my-galaxy))))

(provide 'init-template)
;;; init-template.el ends here.
