;; -*- lexical-binding: t; -*-

(use-package rg
  :ensure t
  :preface (package-activate 'rg)
  :bind ("M-s r" . rg)
  :custom
  (rg-executable "C:/msys64/ucrt64/bin/rg.exe")
  (rg-group-result t)
  (rg-show-columns t))

(with-eval-after-load 'rg
  (add-to-list 'rg-finish-functions (lambda (buffer _) (pop-to-buffer buffer)))
  (add-to-list 'display-buffer-alist '("^\\*rg\\*"
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (window-width . 0.5))))

(use-package ghostel
  :ensure t
  :preface (package-activate 'ghostel)
  :bind ("<f5>" . ghostel))


(provide 'init-prog)
