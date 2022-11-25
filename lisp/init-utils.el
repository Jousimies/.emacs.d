;;; init-utils.el ---  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; Server and restart-emacs
(add-hook 'on-first-input-hook 'server-start)

(require-package 'restart-emacs)

(when (maybe-require-package 'helpful)
  (setq help-window-select t)
  (setq help-window-keep-selected t)
  (global-set-key [remap describe-function] 'helpful-callable)
  (global-set-key [remap describe-variable] 'helpful-variable)
  (global-set-key [remap describe-key] 'helpful-key))

(require-package 'browse-kill-ring)

(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Epkg quick search package and jump to source repo.
(require-package 'epkg)
(require-package 'compat)
(require-package 'closql)
(require-package 'emacsql-sqlite)
(when (require-package 'epkg-marginalia)
  (with-eval-after-load 'marginalia
    (cl-pushnew 'epkg-marginalia-annotate-package
                (alist-get 'package marginalia-annotator-registry))))

(when (maybe-require-package 'bicycle)
  (with-eval-after-load 'outline
    (define-key outline-minor-mode-map (kbd "C-<tab>") 'bicycle-cycle)
    (define-key outline-minor-mode-map (kbd "S-<tab>") 'bicycle-cycle-global)))

;; 有时候会搞乱符号位置，存在 bug 需要解决。
;; (when (maybe-require-package 'parinfer-rust-mode)
;;   (setq parinfer-rust-library (expand-file-name "parinfer-rust/libparinfer_rust.so" user-emacs-directory))
;;   (add-hook 'emacs-lisp-mode-hook 'parinfer-rust-mode)
;;   (with-eval-after-load 'parinfer-rust-mode
;;     (progn
;;       (hungry-delete-mode -1)
;;       (electric-pair-mode -1))))

(when (maybe-require-package 'demap)
  (setq demap-minimap-window-side  'left)
  (setq demap-minimap-window-width 15))

;; (when (maybe-require-package 'reformatter)
;;   (reformatter-define black-format :program "black" :args '("-")))

;; Sometimes warning no format
;; (when (maybe-require-package 'format-all)
;;   (add-hook 'prog-mode-hook 'format-all-mode)
;;   (add-hook 'before-save-hook 'format-all-buffer))

(when (maybe-require-package 'pangu-spacing)
  (add-hook 'org-mode-hook 'pangu-spacing-mode))

(when (maybe-require-package 'whitespace-cleanup-mode)
  (add-hook 'on-first-file-hook 'whitespace-cleanup-mode))


(provide 'init-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
