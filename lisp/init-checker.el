;; -*- lexical-binding: t; -*-

;; jinx
(add-hook 'org-mode-hook 'global-jinx-mode)
(with-eval-after-load 'jinx
  ;; jinx-mod.c is not copied to the build directory.
  ;; Add the source directory so Jinx can locate and compile it.
  (unless (locate-library "jinx-mod.c" t)
    (add-to-list
     'load-path
     (expand-file-name "packages/jinx" user-emacs-directory)
     t))
  (setq jinx-languages "en_US")
  (add-to-list 'jinx-exclude-regexps '(t "\\cc")))


;; flymake
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

(provide 'init-checker)
