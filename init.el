;; init.el --- Personal Emacs Configuration -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp")

(when init-file-debug
  (setq use-package-compute-statistics t)
  (setq use-package-verbose t)
  (require 'init-benchmark))

;; Define some variables to facilitate the location of configuration files or related settings for specific systems.
(defvar mobile-document "~/Library/Mobile Documents/"
  "This folder contains documents in icloud.")

(defvar my-cloud "~/Nextcloud"
  "This folder is My cloud.")

;; L.Personal.Galaxy location may change, but folders in this directory never change.
(defvar my-galaxy (expand-file-name "L.Personal.Galaxy" my-cloud)
  "This folder stores all the plain text files of my life.")

(defvar website-directory (expand-file-name "blogs_source/" my-galaxy)
  "The source folder of my blog.")

(defvar my/web_archive (expand-file-name "web_archive/" my-galaxy)
  "The folder save web pages.")

(require 'init-dashboard)
(require 'init-core)
(require 'init-builtin)
(require 'init-layout)
(require 'init-edit)
(require 'init-completion)
(require 'init-search)
(require 'init-dired)

(require 'init-dict)
(require 'init-lsp)
(require 'init-prog)

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
