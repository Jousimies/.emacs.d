(use-package evil
    :bind (:map evil-motion-state-map
                ("SPC" . nil)
                ("RET" . nil)
                ("TAB" . nil))
    :hook (after-change-major-mode . (lambda ()
                                       (setq-local evil-shift-width tab-width)))
    :init
    (setq evil-want-keybinding nil)
    (setq evil-want-integration t)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-h-delete t)
    (setq evil-respect-visual-line-mode t)
    :config
    (evil-mode)
    (setq evil-undo-system 'undo-fu)
    (setq evil-visual-state-cursor 'hollow)

    (setq evil-normal-state-tag " 𝐍 ")
    (setq evil-insert-state-tag " 𝐈 ")
    (setq evil-motion-state-tag " 𝐌 ")
    (setq evil-visual-state-tag " 𝐕 ")
    (setq evil-replace-state-tag " 𝐑 ")
    (setq evil-operator-state-tag " O ")
    (setq evil-emacs-state-tag " E ")

    (define-key evil-insert-state-map (kbd "C-e") #'move-end-of-line)
    (define-key evil-insert-state-map (kbd "C-k") #'kill-line))

  (global-set-key (kbd "C-M-u") 'universal-argument)
    (with-eval-after-load 'evil
    (evil-define-key '(normal motion visual) 'global
      "ge" nil
      "gn" nil))

(use-package evil-collection
  :after evil
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

(provide 'init-evil)
;;; init-evil.el ends here.
