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

;; Eglot/Flymake
(defgroup my-development nil
  "Project-local development helpers."
  :group 'tools)

(defcustom my/eglot-auto-enable nil
  "Whether to start Eglot automatically when a known server is installed.
When nil, use `M-x my/eglot-ensure-maybe' explicitly."
  :type 'boolean
  :group 'my-development)

(defcustom my/eglot-language-server-candidates
  '(((python-mode python-ts-mode)
     ("pyright-langserver" "--stdio")
     ("pylsp"))
    ((c-mode c-ts-mode c++-mode c++-ts-mode)
     ("clangd"))
    ((rust-mode rust-ts-mode)
     ("rust-analyzer"))
    ((go-mode go-ts-mode)
     ("gopls"))
    ((js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode)
     ("typescript-language-server" "--stdio")))
  "Major modes and language-server command candidates for Eglot.
Only the first candidate whose executable is present is registered."
  :type 'sexp
  :group 'my-development)

(defcustom my/project-format-on-save nil
  "Whether Eglot should format this buffer before saving.
The default is deliberately nil.  Enable it per project with .dir-locals.el,
for example: ((nil . ((my/project-format-on-save . t))))."
  :type 'boolean
  :group 'my-development)
(put 'my/project-format-on-save 'safe-local-variable #'booleanp)

(defun my/eglot-server-spec-for-current-mode ()
  "Return an available Eglot server spec for the current major mode."
  (when-let* ((entry
               (seq-find (lambda (item)
                           (apply #'derived-mode-p (car item)))
                         my/eglot-language-server-candidates)))
    (seq-find (lambda (command)
                (executable-find (car command)))
              (cdr entry))))

(defun my/eglot-ensure-maybe ()
  "Start Eglot only when a configured server executable is available."
  (interactive)
  (unless (and (fboundp 'my/large-file-p) (my/large-file-p))
    (if-let* ((command (my/eglot-server-spec-for-current-mode)))
        (progn
          (require 'eglot)
          (cl-pushnew (cons (list major-mode) command)
                      eglot-server-programs :test #'equal)
          (condition-case err
              (eglot-ensure)
            (error
             (if (called-interactively-p 'interactive)
                 (user-error "Eglot could not start: %s"
                             (error-message-string err))
               (message "Eglot skipped: %s" (error-message-string err))))))
      (when (called-interactively-p 'interactive)
        (user-error "No configured language server found for %s" major-mode)))))

(defun my/eglot-auto-enable-maybe ()
  "Start Eglot when `my/eglot-auto-enable' is non-nil."
  (when my/eglot-auto-enable
    (my/eglot-ensure-maybe)))

(defun my/eglot-format-buffer-maybe ()
  "Format the buffer when explicitly enabled and managed by Eglot."
  (when (and my/project-format-on-save
             (fboundp 'eglot-managed-p)
             (eglot-managed-p))
    (eglot-format-buffer)))

(add-hook 'prog-mode-hook #'my/eglot-auto-enable-maybe 80)
(add-hook 'before-save-hook #'my/eglot-format-buffer-maybe)

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
