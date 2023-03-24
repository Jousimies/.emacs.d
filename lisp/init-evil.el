(use-package general
  :config
  (general-create-definer my/space-leader-def
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :states '(normal visual insert emacs)))

(defun my/emacs-config ()
  "My literate Emacs configuration."
  (interactive)
  (find-file (expand-file-name "emacs.org" user-emacs-directory)))

(global-set-key (kbd "s-,") 'my/emacs-config)

(use-package hydra
  :commands defhydra)

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

(evil-define-key '(normal motion visual) 'global
  "ge" nil
  "gn" nil
  "zx" 'kill-this-buffer)

(use-package evil-commands
  :bind (:map evil-motion-state-map
              ("C-f" . evil-scroll-down)
              ("C-b" . evil-scroll-up)))

(use-package which-key
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))

(provide 'init-evil)
;;; init-evil.el ends here.
