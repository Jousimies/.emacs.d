;; init-evil.el --- Evil Modal editing. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package evil
  :hook ((after-change-major-mode . (lambda ()
                                     (setq-local evil-shift-width tab-width)))
         (after-init . evil-mode))
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-h-delete t)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)
  :config
  (setq evil-visual-state-cursor 'hollow)
  (setq evil-move-beyond-eol t) ;; https://emacs-china.org/t/emacs/19016/131
  (setq evil-want-fine-undo t)
  ;; https://github.com/zsxh/emacs.d/blob/master/lisp/init-evil.el
  ;; remove all keybindings from insert-state keymap,it is VERY VERY important
  (setcdr evil-insert-state-map nil)
  ;; 把emacs模式下的按键绑定到Insert模式下
  (define-key evil-insert-state-map (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
  ;; but [escape] should switch back to normal state
  (define-key evil-insert-state-map [escape] 'evil-normal-state))

(use-package evil-collection
  :hook (evil-mode . evil-collection-init))

(use-package evil-collection-unimpaired
  :after evil-collection
  :diminish evil-collection-unimpaired-mode)

(use-package evil-commentary
  :diminish evil-commentary-mode
  :hook (evil-mode . evil-commentary-mode))

(use-package evil-surround
  :hook (evil-mode . global-evil-surround-mode))

(use-package evil-embrace
  :hook (org-mode . embrace-org-mode-hook)
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-find-char-pinyin
  :diminish evil-find-char-pinyin-mode
  :hook (evil-mode . evil-find-char-pinyin-mode))

(provide 'init-evil)
;;; init-evil.el ends here.
