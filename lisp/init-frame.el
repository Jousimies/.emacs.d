;; init-frame.el --- Frame and window. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(add-to-list 'initial-frame-alist '(alpha . (90 . 100)))
(blink-cursor-mode -1)
;; (use-package frame
;;   :config)

(defun my/make-or-delete-frame ()
  (interactive)
  (if (= (frame-width) 100) ;; 80 is the default frame width.
      (delete-frame)
    (make-frame)))

(global-set-key (kbd "s-n") 'my/make-or-delete-frame)

(use-package winner
  :hook (after-init . winner-mode)
  :config
  (setq winner-dont-bind-my-keys t)
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*esh command on file*")))

(use-package window
  :defer t
  :config
  (setq switch-to-buffer-in-dedicated-window 'pop)
  (setq switch-to-buffer-obey-display-actions t)
  (add-to-list 'display-buffer-alist '("\\*Outline"
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (window-width . 0.5)
                                       (window-parameters
                                        (mode-line-format . none))))
  (add-to-list 'display-buffer-alist '("\\*toc\\*"
                                       (display-buffer-reuse-window display-buffer-in-side-window)
                                       (side . left)
                                       (window-parameters
                                        (mode-line-format . none)
                                        (delete-other-windows . t))))
  (add-to-list 'display-buffer-alist '((derived-mode . prog-mode)
                                       (display-buffer-in-tab)
                                       (tab-name . "Porg") (tab-group . "Prog")
                                       (select . t))))

(use-package frame
  :config
  (face-spec-set 'window-divider
                 '((((background light))
                    :foreground "#000000")
                   (t
                    :foreground "#FFFFFF"))
                 'face-override-spec)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-places 'bottom-only)
  (window-divider-mode 1))

(defun my/scroll-other-windown-down ()
  "Scroll other window down."
  (interactive)
  (scroll-other-window-down 2))

(global-set-key (kbd "M-p") 'my/scroll-other-windown-down)

(defun my/scroll-other-windown ()
  "Scroll other window up."
  (interactive)
  (scroll-other-window 2))

(global-set-key (kbd "M-n") 'my/scroll-other-windown)

(provide 'init-frame)
;;; init-frame.el ends here.
