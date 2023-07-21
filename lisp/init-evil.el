;; init-evil.el --- Evil Modal editing. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package evil
  :bind (:map evil-motion-state-map
              ("SPC" . nil)
              ("RET" . nil)
              ("TAB" . nil))
  :hook ((after-change-major-mode . (lambda ()
                                     (setq-local evil-shift-width tab-width)))
         (after-init . evil-mode))
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-h-delete t)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-visual-state-cursor 'hollow)
  (setq evil-normal-state-tag " 🅝")
  (setq evil-insert-state-tag " 🅘")
  (setq evil-motion-state-tag " 🅜")
  (setq evil-visual-state-tag " 🅥")
  (setq evil-replace-state-tag " 🅡")
  (setq evil-operator-state-tag " 🅞")
  (setq evil-emacs-state-tag " 🅔")
  :config
  (advice-add #'evil-undo :override #'vundo)
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
  :after evil
  :bind (:map evil-motion-state-map
              ("C-f" . evil-scroll-down)
              ("C-b" . evil-scroll-up)))

(use-package evil-collection
  :commands evil-define-key
  :hook (evil-mode . evil-collection-init)
  :config
  (setq evil-collection-key-blacklist '("SPC" ","))
  (setq forge-add-default-bindings nil))

(use-package evil-commentary
  :hook (evil-mode . evil-commentary-mode))

(use-package evil-surround
  :hook (evil-mode . global-evil-surround-mode))

(use-package evil-embrace
  :hook (org-mode . embrace-org-mode-hook)
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-find-char-pinyin
  :hook (evil-mode . evil-find-char-pinyin-mode))

(provide 'init-evil)
;;; init-evil.el ends here.
