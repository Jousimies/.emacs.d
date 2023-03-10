(use-package gc-buffers
  :hook (on-first-buffer . gc-buffers-mode))

(use-package ace-window
  :bind ("C-x o" . ace-window))

(use-package expand-region
  :commands er/expand-region)

(with-eval-after-load 'evil
    (evil-define-key 'visual 'evil-visual-state-map
      "v" 'er/expand-region))

(with-eval-after-load 'outline
  (define-key outline-minor-mode-map (kbd "C-<tab>") 'bicycle-cycle)
  (define-key outline-minor-mode-map (kbd "S-<tab>") 'bicycle-cycle-global))

(provide 'init-edit)
;;; init-edit.el ends here.
