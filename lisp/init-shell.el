;; init-shell.el --- Shell. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package vterm
  :load-path "packages/emacs-libvterm/"
  :bind ("<f5>" . vterm)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 5000))

(global-set-key (kbd "C-<f5>") #'eshell)
(with-eval-after-load 'eshell
  (setopt eshell-directory-name (expand-file-name "eshell" cache-directory)))

(use-package eee
  :load-path "~/.emacs.d/packages/eee.el/"
  :bind-keymap
  ("s-e" . ee-keymap)
  :custom
  (ee-terminal-command "/opt/homebrew/bin/wezterm"))

(with-eval-after-load 'eee
  (defun start-wezterm-at-current-directory ()
    "Start Wezterm at the current buffer's directory."
    (interactive)
    (let ((default-directory (or default-directory "~"))) ; 默认目录为当前 buffer 的目录
      (start-process "wezterm" nil "wezterm" "start" "--cwd" default-directory)))

  (defun switch-to-wezterm ()
    "Switch to WezTerm terminal."
    (interactive)
    (do-applescript "
    tell application \"WezTerm\"
      activate
    end tell"))

  (advice-add 'start-wezterm-at-current-directory :after
              (lambda (&rest _)
		(sleep-for 0.1)
		(switch-to-wezterm)))

  (advice-add 'ee-run :after
              (lambda (&rest _)
		(sleep-for 0.1)
		(switch-to-wezterm))))

(provide 'init-shell)
;;; init-shell.el ends here.
