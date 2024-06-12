;; init.el --- Personal Emacs Configuration -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp")
;; (add-to-list 'load-path "~/.emacs.d/packages/org-mode/lisp/")

(when init-file-debug
  (setq use-package-compute-statistics t)
  (setq use-package-verbose t)
  (require 'init-benchmark))

(require 'init-core)

;; (require 'init-dashboard)

(require 'init-builtin)
;; (require 'init-meow)

(when (eq system-type 'darwin)
  (require 'init-mac))

(require 'init-tab)
(require 'init-modeline)
(require 'init-buffer)

(require 'init-edit)
(require 'init-completion)
(require 'init-search)

(require 'init-dired)

(require 'init-lsp)
(require 'init-prog)
(require 'init-shell)
(require 'init-git)

(require 'init-org)
(require 'init-note)
(require 'init-bib)
(require 'init-gtd)
(require 'init-blog)

(require 'init-dict)

(require 'init-finance)

(require 'init-latex)
(require 'init-reader)
(require 'init-telega)
(require 'init-mail)
(require 'init-elfeed)
(require 'init-music)
(require 'init-pass)

(require 'init-misc)

(require 'init-keys)

;; custom
(load (setq custom-file (locate-user-emacs-file "custom.el")) t)

(provide 'init)
;;; init.el ends here.
