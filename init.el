;; init.el --- Personal Emacs Configuration -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; https://github.com/seagle0128/.emacs.d/blob/master/init.el
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/packages/compat/")
(add-to-list 'load-path "~/.emacs.d/packages/dash.el/")
(add-to-list 'load-path "~/.emacs.d/packages/f.el/")
(add-to-list 'load-path "~/.emacs.d/packages/s.el/")
(add-to-list 'load-path "~/.emacs.d/packages/posframe/")
(add-to-list 'load-path "~/.emacs.d/packages/emacs-async/")

;; https://www.emacswiki.org/emacs/ExecPath
(defun set-exec-path-from-shell-PATH ()
  "This is particularly useful under Mac OS X and macOS."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$" "" (shell-command-to-string
                                          "$SHELL --login -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

(when init-file-debug
  (setq use-package-compute-statistics t)
  (setq use-package-verbose t)
  (require 'init-benchmark))

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
(require 'init-elfeed)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(define-minor-mode minor-mode-blackout-mode
  "Hides minor modes from the mode line."
  :init-value t)
(add-hook 'window-setup-hook #'(lambda ()
                                 (catch 'done
                                   (mapc (lambda (x)
                                           (when (and (consp x)
                                                      (equal (cadr x) '("" minor-mode-alist)))
                                             (let ((original (copy-sequence x)))
                                               (setcar x 'minor-mode-blackout-mode)
                                               (setcdr x (list "" original)))
                                             (throw 'done t)))
                                         mode-line-modes))))

(setq-default initial-scratch-message
              (propertize
               (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you") 'face 'italic))

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
                         "\n\n"))
                90))))

(provide 'init)
;;; init.el ends here.
