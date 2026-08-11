;; -*- lexical-binding: t; -*-

(use-package expreg
  :ensure t
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)))

(provide 'init-edit)
