;;; init-prog.el --- Emacs Programming               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Duan Ning

;; Author: Duan Ning <duan_n@outlook.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
;; (use-package magit-status
;;   :load-path ("packages/magit/lisp" "packages/with-editor/lisp" "packages/llama/")
;;   :bind ("C-x g" . magit-status-quick))

;; (with-eval-after-load 'magit
;;   (setopt magit-git-executable "/usr/bin/git")
;;   (magit-add-section-hook 'magit-status-sections-hook
;;                           'magit-insert-modules
;;                           'magit-insert-unpulled-from-upstream)
;;   (remove-hook 'magit-module-sections-hook 'magit-insert-modules-overview)
;;   (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpulled-from-pushremote)
;;   (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-upstream)
;;   (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-pushremote))

(with-eval-after-load 'transient
  (setq transient-history-file (expand-file-name "transient/history.el" cache-directory)
	transient-levels-file (expand-file-name "transient/levels.el" cache-directory)
	transient-values-file (expand-file-name "transient/values.el" cache-directory)))

(use-package git-timemachine
  :load-path "packages/git-timemachine/"
  :bind ("M-g t" . git-timemachine))

(use-package browse-at-remote
  :load-path "packages/browse-at-remote/"
  :bind ("M-g b" . browse-at-remote))

;; Mac 上需要先安装 tree-sitter 然后再编译 Emacs
;; brew install tree-sitter
(with-eval-after-load 'treesit
  (setopt treesit-language-source-alist
	  '((python "https://github.com/tree-sitter/tree-sitter-python.git")
	    (yaml "https://github.com/ikatyang/tree-sitter-yaml.git")
	    (typescript "https://github.com/tree-sitter/tree-sitter-typescript.git")
	    (json "https://github.com/tree-sitter/tree-sitter-json.git")
	    (csharp "https://github.com/tree-sitter/csharp-tree-sitter.git"))))

;; eglot
(add-hook 'LaTeX-mode-hook #'eglot-ensure)
(add-hook 'python-ts-mode #'eglot-ensure)
(add-hook 'csharp-ts-mode #'eglot-ensure)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(csharp-mode . ("~/Downloads/omnisharp-osx-arm64-net6/OmniSharp" "-lsp"))))

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; markdown
(use-package markdown-mode
  :load-path "packages/markdown-mode/"
  :mode (("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
	      ("C-c C-e" . markdown-do)))

;; swift
(use-package swift-mode
  :load-path "packages/swift-mode/"
  :mode ("\\.swift\\'" . swift-mode))

;; yaml
(add-to-list 'auto-mode-alist '("\\.yaml\\|\\.yml\\'" . yaml-ts-mode))
;; (use-package yaml-ts-mode
;;   :mode ("\\.yaml\\|\\.yml\\'" . yaml-ts-mode))

;; csv
(use-package csv-mode
  :load-path "packages/csv-mode/"
  :mode ("\\.csv\\'" . csv-mode))

(use-package rainbow-csv
  :load-path "packages/rainbow-csv/"
  :hook ((csv-mode . rainbow-csv-mode)
	 (tsv-mode . rainbow-csv-mode)))

;; (use-package demap
;;   :load-path "~/.emacs.d/packages/demap.el/"
;;   :hook (prog-mode . demap-toggle)
;;   :config
;;   (setq demap-minimap-window-side  'right)
;;   (setq demap-minimap-window-width 15))

(use-package visual-basic-mode
  :mode ("\\.vb\\|.bas\\'" . visual-basic-mode))

(use-package lua-mode
  :load-path "packages/lua-mode/"
  :mode "\\.lua$"
  :interpreter "lua")

(use-package vterm
  :load-path "packages/emacs-libvterm/"
  :bind ("<f5>" . vterm)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 5000))

(use-package eee
  :load-path "~/.emacs.d/packages/eee.el/"
  :bind-keymap ("s-e" . ee-keymap)
  :custom
  (ee-terminal-command "/opt/homebrew/bin/wezterm"))

(with-eval-after-load 'eee
  (defun start-wezterm-at-current-directory ()
    "Start Wezterm at the current buffer's directory."
    (interactive)
    (let ((default-directory (or default-directory "~"))) ; 默认目录为当前 buffer 的目录
      (start-process "wezterm" nil "wezterm" "start" "--cwd" default-directory)))

  (advice-add 'start-wezterm-at-current-directory :after
              (lambda (&rest _)
		(sleep-for 0.1)
		(switch-to-wezterm)))

  (advice-add 'ee-run :after
              (lambda (&rest _)
		(sleep-for 0.1)
		(switch-to-wezterm))))

(defun blog-git-auto-push ()
  "Automatically stage, commit, and push changes to the blog repository."
  (interactive)
  (let* ((blog-dir "~/blog/") ; 替换为你的博客目录
         (commit-message (format-time-string "Blog update %Y-%m-%d %H:%M:%S")))
    (save-some-buffers t) ; 保存所有未保存的缓冲区
    (shell-command (concat "cd " blog-dir " && git add ."))
    (shell-command (concat "cd " blog-dir " && git commit -m \"" commit-message "\""))
    (shell-command (concat "cd " blog-dir " && git push origin main"))
    (message "Blog changes staged, committed, and pushed!")))

(provide 'init-prog)
;;; init-prog.el ends here
