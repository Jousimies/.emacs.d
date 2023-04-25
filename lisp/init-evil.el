;; init-evil.el --- Evil Modal editing. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package general
  :config
  (general-create-definer my/space-leader-def
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :states '(normal visual insert emacs)))

(my/space-leader-def
  "r" 'consult-recent-file)

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
  (setq evil-undo-system 'undo-fu)
  (setq evil-visual-state-cursor 'hollow)
  (setq evil-normal-state-tag " 𝐍 ")
  (setq evil-insert-state-tag " 𝐈 ")
  (setq evil-motion-state-tag " 𝐌 ")
  (setq evil-visual-state-tag " 𝐕 ")
  (setq evil-replace-state-tag " 𝐑 ")
  (setq evil-operator-state-tag " O ")
  (setq evil-emacs-state-tag " E ")

  :config
  (evil-mode)

  ;; https://github.com/zsxh/emacs.d/blob/master/lisp/init-evil.el
  ;; remove all keybindings from insert-state keymap,it is VERY VERY important
  (setcdr evil-insert-state-map nil)
  ;; 把emacs模式下的按键绑定到Insert模式下
  (define-key evil-insert-state-map (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
  ;; but [escape] should switch back to normal state
  (define-key evil-insert-state-map [escape] 'evil-normal-state)

  (evil-define-key '(normal motion visual) 'global
    "ge" nil
    "gn" nil))

(use-package evil-commands
  :bind (:map evil-motion-state-map
              ("C-f" . evil-scroll-down)
              ("C-b" . evil-scroll-up)))

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

(provide 'init-evil)
;;; init-evil.el ends here.
