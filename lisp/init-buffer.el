;; init-buffer.el --- Buffer manipulate. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(defun switch-to-message ()
  "Quick switch to `*Message*' buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package gc-buffers
  :config
  (gc-buffers-mode))

(with-eval-after-load 'outline
  (define-key outline-minor-mode-map (kbd "C-<tab>") 'bicycle-cycle)
  (define-key outline-minor-mode-map (kbd "S-<tab>") 'bicycle-cycle-global))

(use-package midnight
  :config
  (midnight-mode))

(use-package word-wrap-mode
  :hook (org-mode . word-wrap-whitespace-mode))

(provide 'init-buffer)
;;; init-buffer.el ends here.
