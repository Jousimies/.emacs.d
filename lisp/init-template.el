(use-package tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :config
  (setq tempel-path `("~/.emacs.d/template/tempel"
                      ,(expand-file-name "template/tempel" my-galaxy))))

(use-package yasnippet
  :hook (on-first-file . yas-global-mode)
  :config
  (use-package yasnippet-snippets
    :after yasnippet))

(provide 'init-template)
;;; init-template.el ends here.
