;; init-buffer.el --- Buffer manipulate. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(defun switch-to-message ()
  "Quick switch to `*Message*' buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(keymap-global-set "C-c b m" 'switch-to-message)

(my/space-leader-def
  "b" '(:ignore t :wk "Buffer")
  "bm" '(switch-to-message :wk "*message*"))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-default-sorting-mode 'major-mode))

(use-package ibuf-ext
  :hook (ibuffer-mode . ibuffer-auto-mode))

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
