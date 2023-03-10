(use-package evil
  :bind ((:map evil-insert-state-map
              ("C-e" . move-end-of-line)
              ("C-k" . kill-line))
         (:map evil-motion-state-map
               ("SPC" . nil)
               ("RET" . nil)
               ("TAB" . nil)))
  :hook ((after-init . evil-mode)
         (after-change-major-mode . (lambda ()
                                      (setq-local evil-shift-width tab-width))))
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-h-delete t)
  (setq evil-respect-visual-line-mode t)
  :config
  (setq evil-undo-system 'undo-fu)
  (setq evil-visual-state-cursor 'hollow)

  (setq evil-normal-state-tag " 𝐍 ")
  (setq evil-insert-state-tag " 𝐈 ")
  (setq evil-motion-state-tag " 𝐌 ")
  (setq evil-visual-state-tag " 𝐕 ")
  (setq evil-replace-state-tag " 𝐑 ")
  (setq evil-operator-state-tag " O ")
  (setq evil-emacs-state-tag " E "))

(global-set-key (kbd "C-M-u") 'universal-argument)

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-key-blacklist '("SPC" ","))
  (setq forge-add-default-bindings nil)
  (evil-collection-init))

(use-package evil-commentary
  :hook (on-first-file . evil-commentary-mode))

(use-package evil-surround
  :hook (on-first-file . global-evil-surround-mode))

(use-package evil-embrace
  :hook (org-mode . embrace-org-mode-hook)
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-easymotion
  :after evil
  :config
  (evilem-default-keybindings "SPC"))

(use-package which-key
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05))

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

(my/space-leader-def
  "f" '(:ignore t :wk "Files"))

(with-eval-after-load 'evil
  (evil-define-key '(normal motion visual) 'global
    "ge" nil
    "gn" nil))

(provide 'init-evil)
;;; init-evil.el ends here.
