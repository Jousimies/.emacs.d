;; init-checker.el --- Checker. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package jinx
  :hook (org-mode . jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (setq jinx-languages '("en" "fr" "de"))
  (add-to-list 'jinx-exclude-regexps '(t "\\cc")))

(provide 'init-checker)
;;; init-checker.el ends here.
