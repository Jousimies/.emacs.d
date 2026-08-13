;; -*- lexical-binding: t; -*-

(use-package expreg
  :ensure t
  :preface (package-activate 'expreg)
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)))


(provide 'init-edit)
