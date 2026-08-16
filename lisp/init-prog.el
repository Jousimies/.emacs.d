;; -*- lexical-binding: t; -*-

;; treesit
;; grammar可以从这下载：https://github.com/emacs-tree-sitter/tree-sitter-langs
;; 不同的 Emacs 版本需要不同 ABI 版本的 grammar, ABI 通过 (treesit-library-abi-version) 查看
;; 下载后的文件需要改名，如 yaml.dll->libtree-sitter-yaml.dll
;; 否则 (treesit-language-available-p 'yaml t) 会报错
(with-eval-after-load 'treesit
  (add-to-list 'treesit-extra-load-path
               (expand-file-name "tree-sitter" user-emacs-directory)))

(setq major-mode-remap-alist
      '((python-mode     . python-ts-mode)
        (js-mode         . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (css-mode        . css-ts-mode)
        (html-mode       . html-ts-mode)
        (json-mode       . json-ts-mode)
        (c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (yaml-mode       . yaml-ts-mode)
        (rust-mode       . rust-ts-mode)
        (go-mode         . go-ts-mode)))

;; Windows 11 可以通过 winget 安装 ripgrep
;; winget search ripgrep
;; winget install BurntSushi.ripgrep.MSVC
;; rg.exe 的路径在环境变量 path 中，(executable-find "rg") 就可以找到路径
(use-package rg
  :ensure t
  :bind ("M-s r" . rg)
  :custom
  (rg-executable (executable-find "rg"))
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
  :bind ("<f5>" . ghostel))

;; markdown
(use-package markdown-mode
  :ensure t
  :mode (("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
	      ("C-c C-e" . markdown-do)))

(use-package lua-mode
  :ensure t
  :mode "\\.lua$"
  :interpreter "lua")

;; yaml
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

(provide 'init-prog)
