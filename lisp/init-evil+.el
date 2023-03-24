(use-package evil-collection
  :config
  (setq evil-collection-key-blacklist '("SPC" ","))
  (setq forge-add-default-bindings nil)
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode))

(use-package evil-embrace
  :hook (org-mode . embrace-org-mode-hook)
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-easymotion
  :after evil
  :config
  (evilem-default-keybindings "SPC"))

(use-package anzu
  :config
  (global-anzu-mode 1))

(use-package evil-anzu
  :after evil anzu)

(use-package evil-find-char-pinyin
  :after evil
  :config
  (evil-find-char-pinyin-mode 1))

(provide 'init-evil+)
;;; init-evil+.el ends here.
