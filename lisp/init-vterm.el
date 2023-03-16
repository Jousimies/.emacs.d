;; init-vterm.el --- vterm *- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:
(use-package vterm
  :general (my/space-leader-def
             "v" '(toggle-vterm :wk "vterm"))
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000)
  (add-to-list 'display-buffer-alist
               '("\\*vterm\\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.35)
                 (side . bottom)
                 (slot . -1)))
  (defun toggle-vterm ()
    "Toggle vterm on or off."
    (interactive)
    (if (get-buffer-window "*vterm*")
        (delete-window (get-buffer-window "*vterm*"))
      (progn
        (vterm)
        (evil-insert 1)))))

(provide 'init-vterm)
;;; init-vterm.el ends here.
