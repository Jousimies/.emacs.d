;; -*- lexical-binding: t; -*-

;; treesit
;; grammar可以从这下载：https://github.com/emacs-tree-sitter/tree-sitter-langs
;; 不同的 Emacs 版本需要不同 ABI 版本的 grammar, ABI 通过 (treesit-library-abi-version) 查看
;; 下载后的文件需要改名，如 yaml.dll->libtree-sitter-yaml.dll
;; 否则 (treesit-language-available-p 'yaml t) 会报错

(with-eval-after-load 'treesit
  (add-to-list 'treesit-extra-load-path
               (expand-file-name "module/tree-sitter" user-emacs-directory)))
(setq treesit-enabled-modes t)

;; (setq major-mode-remap-alist
;;       '((python-mode     . python-ts-mode)
;;         (js-mode         . js-ts-mode)
;;         (typescript-mode . typescript-ts-mode)
;;         (css-mode        . css-ts-mode)
;;         (html-mode       . html-ts-mode)
;;         (json-mode       . json-ts-mode)
;;         (c-mode          . c-ts-mode)
;;         (c++-mode        . c++-ts-mode)
;;         (yaml-mode       . yaml-ts-mode)
;;         (rust-mode       . rust-ts-mode)
;;         (go-mode         . go-ts-mode)))


;; goggles
(add-hook 'prog-mode-hook #'goggles-mode)
(add-hook 'text-mode-hook #'goggles-mode)
(with-eval-after-load 'goggles
  (setq-default goggles-pulse t))

;; form-feed
(add-hook 'on-first-buffer-hook #'global-form-feed-mode)

;; rainbow-mode
(add-hook 'prog-mode-hook #'rainbow-mode)

;; Windows 11 可以通过 winget 安装 ripgrep
;; winget search ripgrep
;; winget install BurntSushi.ripgrep.MSVC
;; rg.exe 的路径在环境变量 path 中，(executable-find "rg") 就可以找到路径
(global-set-key (kbd "M-s r") #'rg)
(with-eval-after-load 'rg
  (setq rg-executable (executable-find "rg"))
  (setq rg-group-result t)
  (setq rg-show-columns t))

(with-eval-after-load 'rg
  (add-to-list 'rg-finish-functions (lambda (buffer _) (pop-to-buffer buffer)))
  (add-to-list 'display-buffer-alist '("^\\*rg\\*"
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (window-width . 0.5))))


;; Python
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))


;; markdown
(dolist (re '("\\.md\\'" "\\.mdx\\'" "\\.markdown\\'"))
  (add-to-list 'auto-mode-alist (cons re 'markdown-ts-mode)))


;; lua-mode
(add-to-list 'major-mode-remap-alist '(lua-mode . lua-ts-mode))


;; yaml
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

(provide 'init-prog)
