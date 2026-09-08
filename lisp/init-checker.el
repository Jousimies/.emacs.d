;; -*- lexical-binding: t; -*-

;; jinx
(defun my/jinx-mode-maybe ()
  "Enable `jinx-mode' in interactive Org buffers."
  (unless noninteractive
    (jinx-mode 1)))

(add-hook 'org-mode-hook #'my/jinx-mode-maybe)
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
