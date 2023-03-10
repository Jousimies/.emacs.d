(use-package helpful
  :commands helpful-update
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key))
  :init
  (setq help-window-select 'always)
  (setq help-window-keep-selected t)
  :config
  (add-to-list 'display-buffer-alist
               '((or (derived-mode . help-mode)
                     (derived-mode . helpful-mode))
                 (display-buffer-reuse-mode-window display-buffer-in-side-window)
                 (window-width . 0.5)
                 (side . right)
                 (slot . 0))))

(autoload #'elisp-demos-advice-helpful-update "elisp-demos" nil t)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(provide 'init-helpful)
;;; init-helpful.el ends here.
