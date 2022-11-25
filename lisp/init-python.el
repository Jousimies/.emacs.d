;;; init-python.el --- Python. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(require-package 'pip-requirements)

(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . toml-mode)))

;; (when (maybe-require-package 'reformatter)
;;   (reformatter-define black :program "black" :args '("-")))


(provide 'init-python)
;;; init-python.el ends here
