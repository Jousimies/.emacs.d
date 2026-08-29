;; -*- lexical-binding: t; -*-

(add-hook 'on-first-buffer-hook 'viper-mode)
(setq viper-mode t
      viper-expert-level 5
      viper-inhibit-startup-message t
      viper-always-drop-to-ask nil
      viper-ex-style-motion nil
      viper-ex-style-editing nil
      viper-insert-state-cursor-color "orange"
      viper-suppress-input-method-change-message t
      viper-auto-indent t)

;;; Viper 光标色：与主题 / 系统明暗同步

(defun my-viper-force-cursor (color)
  "把 Viper 相关光标色全部设为 COLOR（字符串）。
同时更新：全局变量、frame 参数、saved 缓存，并按当前状态立刻应用。"
  (interactive "sCursor color: ")
  (unless (and (stringp color) (color-defined-p color))
    (error "Invalid color: %s" color))
  (when (boundp 'viper-vi-state-cursor-color)
    (setq viper-vi-state-cursor-color color)
    ;; insert / emacs / replace 也可跟主题走；若只要 vi 变、其它固定，可改这里
    (setq viper-insert-state-cursor-color color
          viper-emacs-state-cursor-color  color
          ;; replace 仍用醒目色的话改成 (or (face-foreground 'error nil t) "Red")
          viper-replace-overlay-cursor-color color)
    (dolist (frame (frame-list))
      (modify-frame-parameters
       frame
       `((cursor-color . ,color)
         (viper-vi-state-cursor-color . ,color)
         (viper-insert-state-cursor-color . ,color)
         (viper-emacs-state-cursor-color . ,color)
         (viper-replace-overlay-cursor-color . ,color)
         ;; 关键：清掉旧主题留下的 restore 缓存
         (viper-saved-cursor-color-in-insert-mode . ,color)
         (viper-saved-cursor-color-in-replace-mode . ,color)
         (viper-saved-cursor-color-in-emacs-mode . ,color))))
    (when (fboundp 'viper-set-cursor-color-according-to-state)
      (viper-set-cursor-color-according-to-state))))

(defun my-viper-cursor-color-from-theme ()
  "从当前主题取应用光标色：优先 cursor face，否则 frame。"
  (or (face-background 'cursor nil t)
      (frame-parameter nil 'cursor-color)
      (face-foreground 'default nil t)))

(defun my-viper-sync-cursor-from-theme (&rest _)
  "主题启用后，把 Viper 光标同步到当前主题的 cursor 色。"
  (when (bound-and-true-p viper-mode)
    (let ((color (my-viper-cursor-color-from-theme)))
      (when (and (stringp color) (color-defined-p color))
        (my-viper-force-cursor color)))))

(when (boundp 'enable-theme-functions)
  (add-hook 'enable-theme-functions #'my-viper-sync-cursor-from-theme))

(with-eval-after-load 'modus-themes
  (add-hook 'modus-themes-after-load-theme-hook
            #'my-viper-sync-cursor-from-theme))

(defun my-apply-theme-for-appearance (appearance)
  (my-viper-sync-cursor-from-theme))

(when (boundp 'ns-system-appearance-change-functions)
  (add-hook 'ns-system-appearance-change-functions
            #'my-apply-theme-for-appearance))

(add-hook 'viper-insert-state-hook (lambda ()
				     (setq-local cursor-type 'bar)))
(add-hook 'viper-vi-state-hook (lambda ()
				     (setq-local cursor-type 'box)))

(when (eq system-type 'windows-nt)
  (add-hook 'viper-vi-state-hook (lambda () (w32-set-ime-open-status nil))))

(with-eval-after-load 'biblio
  (add-to-list 'viper-emacs-state-mode-list 'biblio-selection-mode))
;; (use-package meow
;;   :commands meow-global-mode
;;   :config
;;   (meow-motion-define-key
;;    '("j" . meow-next)
;;    '("k" . meow-prev)
;;    '("<escape>" . ignore))
;;   (meow-leader-define-key
;;    ;; Use SPC (0-9) for digit arguments.
;;    '("1" . meow-digit-argument)
;;    '("2" . meow-digit-argument)
;;    '("3" . meow-digit-argument)
;;    '("4" . meow-digit-argument)
;;    '("5" . meow-digit-argument)
;;    '("6" . meow-digit-argument)
;;    '("7" . meow-digit-argument)
;;    '("8" . meow-digit-argument)
;;    '("9" . meow-digit-argument)
;;    '("0" . meow-digit-argument)
;;    '("/" . meow-keypad-describe-key)
;;    '("?" . meow-cheatsheet))
;;   (meow-normal-define-key
;;    '("0" . meow-expand-0)
;;    '("9" . meow-expand-9)
;;    '("8" . meow-expand-8)
;;    '("7" . meow-expand-7)
;;    '("6" . meow-expand-6)
;;    '("5" . meow-expand-5)
;;    '("4" . meow-expand-4)
;;    '("3" . meow-expand-3)
;;    '("2" . meow-expand-2)
;;    '("1" . meow-expand-1)
;;    '("-" . negative-argument)
;;    '(";" . meow-reverse)
;;    '("," . meow-inner-of-thing)
;;    '("." . meow-bounds-of-thing)
;;    '("[" . meow-beginning-of-thing)
;;    '("]" . meow-end-of-thing)
;;    '("a" . meow-append)
;;    '("A" . meow-open-below)
;;    '("b" . meow-back-word)
;;    '("B" . meow-back-symbol)
;;    '("c" . meow-change)
;;    '("d" . meow-delete)
;;    '("D" . meow-backward-delete)
;;    '("e" . meow-next-word)
;;    '("E" . meow-next-symbol)
;;    '("f" . meow-find)
;;    '("g" . meow-cancel-selection)
;;    '("G" . meow-grab)
;;    '("h" . meow-left)
;;    '("H" . meow-left-expand)
;;    '("i" . meow-insert)
;;    '("I" . meow-open-above)
;;    '("j" . meow-next)
;;    '("J" . meow-next-expand)
;;    '("k" . meow-prev)
;;    '("K" . meow-prev-expand)
;;    '("l" . meow-right)
;;    '("L" . meow-right-expand)
;;    '("m" . meow-join)
;;    '("n" . meow-search)
;;    '("o" . meow-block)
;;    '("O" . meow-to-block)
;;    '("p" . meow-yank)
;;    '("q" . meow-quit)
;;    '("Q" . meow-goto-line)
;;    '("r" . meow-replace)
;;    '("R" . meow-swap-grab)
;;    '("s" . meow-kill)
;;    '("t" . meow-till)
;;    '("u" . meow-undo)
;;    '("U" . meow-undo-in-selection)
;;    '("v" . meow-visit)
;;    '("w" . meow-mark-word)
;;    '("W" . meow-mark-symbol)
;;    '("x" . meow-line)
;;    '("X" . meow-goto-line)
;;    '("y" . meow-save)
;;    '("Y" . meow-sync-grab)
;;    '("z" . meow-pop-selection)
;;    '("'" . repeat)
;;    '("<escape>" . ignore))
;;   (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty))

;; (add-hook 'on-init-ui-hook #'meow-global-mode)


(provide 'init-modal)
