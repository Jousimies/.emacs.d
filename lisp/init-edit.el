;; -*- lexical-binding: t; -*-

;; (use-package expreg
;;   :ensure t
;;   :bind (("C-=" . expreg-expand)
;;          ("C--" . expreg-contract)))

(setup (:warm expreg)
  (:when-loaded
    (keymap-global-set "C-=" 'expreg-expand)
    (keymap-global-set "C--" 'expreg-contract)))


(provide 'init-edit)
