;; init-input-method.el --- Input methods. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;; Earlier, I use emacs-rime. Now, sis I selected.

;;; Code:

(use-package sis
  :config
  (setq sis-other-cursor-color "red")
  (face-spec-set 'sis-inline-face
               '((((background light))
                  :foreground "black" :background "#94d4ff")
                 (t
                  :foreground "black" :background "#a4d5f9"))
               'face-override-spec)

  (setq sis-external-ism "im-select")
  (sis-ism-lazyman-config "com.apple.keylayout.ABC" "im.rime.inputmethod.Squirrel.Hans")
  (sis-global-cursor-color-mode t)
  (sis-global-context-mode t)
  (sis-global-respect-mode t)
  (sis-global-inline-mode t)
  (add-to-list 'sis-context-detectors
               (lambda (&rest _)
                 ;; (when (or (and (eq major-mode 'org-mode) (org-at-heading-p))
                 (when (or (eq major-mode 'org-mode)
                           (eq major-mode 'telega-chat-mode))
                   'other))))


(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (setq sis-default-cursor-color (foreground-color-at-point))))

(defun my/apply-theme-and-cursor (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (progn
                (load-theme 'modus-operandi t)
                (setq sis-default-cursor-color (foreground-color-at-point))))
      ('dark (progn
               (load-theme 'modus-vivendi t)
               (setq sis-default-cursor-color (foreground-color-at-point))))))

(advice-add 'my/apply-theme :override 'my/apply-theme-and-cursor)

;; Search file with first char.
(use-package pinyinlib
  :after orderless
  :config
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))

  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

(provide 'init-input-method)
;;; init-input-method.el ends here.
