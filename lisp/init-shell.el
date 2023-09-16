;; init-shell.el --- Shell. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package vterm-toggle
  :load-path ("packages/vterm-toggle/" "packages/emacs-libvterm")
  :bind (("<f5>" . vterm-toggle)
         ("C-<f5>" . vterm-toggle-cd))
  :init
  (add-to-list 'display-buffer-alist
                 '("^\\*vterm"
                   (display-buffer-in-side-window)
                   (window-height . 0.5)
                   (side . bottom)
                   (slot . -1))))

(provide 'init-shell)
;;; init-shell.el ends here.
