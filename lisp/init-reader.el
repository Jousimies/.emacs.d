;; -*- lexical-binding: t; -*-

(use-package nov
  :mode (".epub" . nov-mode)
  :custom
  (nov-unzip-program (executable-find "bsdtar"))
  (nov-unzip-args '("-xC" directory "-f" filename))
  (nov-save-place-file (expand-file-name "nov_place" cache-directory)))


(provide 'init-reader)
