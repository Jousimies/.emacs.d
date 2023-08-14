;; init-shell.el --- Shell. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package vterm
  :init
  (add-to-list 'display-buffer-alist
               '("^\\*vterm"
                 (display-buffer-in-side-window)
                 (window-height . 0.5)
                 (side . bottom)
                 (slot . -1)))
  :bind ("<f3>" . toggle-vterm)
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000)
  (defun toggle-vterm ()
    "Toggle vterm."
    (interactive)
    (let ((buf (concat "*vterm-" (buffer-name))))
      (if (get-buffer buf)
          (switch-to-buffer buf)
        (switch-to-buffer (get-buffer-create buf))
        (vterm-mode)))))

(provide 'init-shell)
;;; init-shell.el ends here.
