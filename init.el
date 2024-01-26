;; init.el --- Personal Emacs Configuration -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp")

(when init-file-debug
  (setq use-package-compute-statistics t)
  (setq use-package-verbose t)
  (require 'init-benchmark))

(require 'init-core)

;; (require 'init-dashboard)

(require 'init-builtin)

(require 'init-mac)

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

;; (unless (featurep 'dashboard)
;;   ;; show startup time at `scratch' buffer.
;;   (defun my/packages-installed (load-path)
;; 	(let ((my/packages 0))
;;       (dolist (path load-path)
;; 		(when (not (string-prefix-p "/Applications/" path))
;;           (setq my/packages (1+ my/packages))))
;;       my/packages))

;;   (add-hook 'window-setup-hook
;; 			(lambda ()
;;               (garbage-collect)
;;               (let ((curtime (current-time)))
;; 				(with-current-buffer "*scratch*"
;;                   (goto-char (point-max))
;;                   (insert
;;                    (concat "\n"
;;                            (format ";; Emacs Startup Times: init:%.03f total:%.03f gc-done:%d"
;;                                    (float-time (time-subtract after-init-time before-init-time))
;;                                    (float-time (time-subtract curtime before-init-time))
;;                                    gcs-done)
;;                            "\n"
;;                            (format ";; Total Packages Required: %d" (my/packages-installed load-path))
;;                            "\n\n"))
;;                   90)))))

(provide 'init)
;;; init.el ends here.
