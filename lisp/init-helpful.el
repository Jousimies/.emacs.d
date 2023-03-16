(use-package help
  :config
  (setq help-window-select 'always)
  (setq help-window-keep-selected t))

(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)))

(autoload #'elisp-demos-advice-helpful-update "elisp-demos" nil t)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(provide 'init-helpful)
;;; init-helpful.el ends here.
