;;; init-utils.el ---  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; Server and restart-emacs
(server-mode)
(require-package 'restart-emacs)

(general-define-key
 :keymaps '(normal visual emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "q" '(:ignore t :wk "Quit/Restart")
 "qR" '(restart-emacs :wk "Restart emacs"))

(when (maybe-require-package 'helpful)
  (global-set-key [remap describe-function] 'helpful-callable)
  (global-set-key [remap describe-variable] 'helpful-variable)
  (global-set-key [remap describe-key] 'helpful-key))

(when (maybe-require-package 'browse-kill-ring)
  (browse-kill-ring-default-keybindings))

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

(general-define-key
 :states '(normal visual emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 ";e" '(epkg-describe-package :wk "Epkg"))

;; websocket-bridge
;; (require 'websocket-bridge)

(when (maybe-require-package 'bicycle)
  (with-eval-after-load 'outline
    (define-key outline-minor-mode-map (kbd "C-<tab>") 'bicycle-cycle)
    (define-key outline-minor-mode-map (kbd "S-<tab>") 'bicycle-cycle-global)))

;; (when (maybe-require-package 'parinfer-rust-mode)
;;   (setq parinfer-rust-library (expand-file-name "parinfer-rust/libparinfer_rust.so" user-emacs-directory))
;;   (add-hook 'emacs-lisp-mode-hook 'parinfer-rust-mode)
;;   (with-eval-after-load 'parinfer-rust-mode
;;     (global-hungry-delete-mode -1)
;;     (electric-pair-mode -1)))

(when (maybe-require-package 'demap)
  (setq demap-minimap-window-side  'left)
  (setq demap-minimap-window-width 15)
  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "tm" '(demap-toggle :wk "Minimap")))

;; (when (maybe-require-package 'reformatter)
;;   (reformatter-define black :program "black" :args '("-")))

(when (maybe-require-package 'format-all)
  (add-hook 'prog-mode-hook 'format-all-mode)
  (add-hook 'before-save-hook 'format-all-buffer))

(provide 'init-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
