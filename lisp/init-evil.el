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
  ".i" '(my/emacs-config :wk "Configuration"))

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
(evil-define-key '(normal motion visual) 'global
  "ge" nil
  "gn" nil
  "zx" 'kill-this-buffer)

(provide 'init-evil)
;;; init-evil.el ends here.
