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

;; markdown
(use-package markdown-mode
  :ensure t
  :preface (package-activate 'markdown-mode)
  :mode (("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
	      ("C-c C-e" . markdown-do)))

(use-package lua-mode
  :ensure t
  :preface (package-activate 'lua-mode)
  :mode "\\.lua$"
  :interpreter "lua")

(provide 'init-prog)
