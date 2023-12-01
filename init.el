;; init.el --- Personal Emacs Configuration -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/packages/compat/")
(add-to-list 'load-path "~/.emacs.d/packages/dash.el/")
(add-to-list 'load-path "~/.emacs.d/packages/f.el/")
(add-to-list 'load-path "~/.emacs.d/packages/s.el/")
(add-to-list 'load-path "~/.emacs.d/packages/posframe/")
(add-to-list 'load-path "~/.emacs.d/packages/emacs-async/")

(when init-file-debug
  (setq use-package-compute-statistics t)
  (setq use-package-verbose t)
  (require 'init-benchmark))

(require 'init-dashboard)

(require 'init-base)
(require 'init-ui)
;; (require 'init-meow)
(require 'init-crud)
(require 'init-completion)
(require 'init-dired)
(require 'init-search)
(require 'init-dict)
(require 'init-lsp)
(require 'init-git)
(require 'init-org)
(require 'init-org+)
(require 'init-note)
(require 'init-bib)
(require 'init-gtd)
(require 'init-reader)
(require 'init-shell)
(require 'init-misc)
(require 'init-latex)
(require 'init-finance)
(require 'init-telega)
(require 'init-markdown)
(require 'init-mail)
;; (require 'init-elfeed)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setq-default initial-scratch-message
              (propertize
               (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you") 'face 'italic))

(defun my/packages-installed (load-path)
  (let ((my/packages 0))
    (dolist (path load-path)
      (when (not (string-prefix-p "/Applications/" path))
        (setq my/packages (1+ my/packages))))
    my/packages))

(add-hook 'window-setup-hook
          (lambda ()
            (garbage-collect)
            (let ((curtime (current-time)))
              (with-current-buffer "*scratch*"
                (goto-char (point-max))
                (insert
                 (concat "\n"
                         (format ";; Emacs Startup Times: init:%.03f total:%.03f gc-done:%d"
                                 (float-time (time-subtract after-init-time before-init-time))
                                 (float-time (time-subtract curtime before-init-time))
                                 gcs-done)
                         "\n"
                         (format ";; Total Packages Required: %d" (my/packages-installed load-path))
                         "\n\n"))
                90))))

(provide 'init)
;;; init.el ends here.
