;; init.el --- Personal Emacs Configuration -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp")

(when init-file-debug
  (setq use-package-compute-statistics t)
  (setq use-package-verbose t)
  (require 'init-benchmark))

(require 'init-core)

(require 'init-builtin)

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
(require 'init-reader)
(require 'init-dict)
(require 'init-latex)

(require 'init-finance)

(require 'init-telega)
(require 'init-mail)
(require 'init-elfeed)

;; custom
(load (setq custom-file (locate-user-emacs-file "custom.el")) t)

(provide 'init)
;;; init.el ends here.
