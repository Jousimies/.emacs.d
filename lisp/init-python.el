;;; init-python.el --- Python. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq python-shell-interpreter "python3")

(require-package 'pip-requirements)

(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . toml-mode)))

(when (maybe-require-package 'reformatter)
  (reformatter-define black :program "black" :args '("-")))


(provide 'init-python)
;;; init-python.el ends here
