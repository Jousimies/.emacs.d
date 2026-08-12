;; -*- lexical-binding: t; -*-

;; Package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(setq package-check-signature nil)

;; Setup and on.el
(unless (package-installed-p 'setup)
  (package-install 'setup))
(setup (:package on))

;; Benchmark
;; (setup (:package benchmark-init)
;;   (:require benchmark-init)
;;   (:when-loaded
;;     (add-hook 'after-init-hook 'benchmark-init/deactivate)))

;; Require configurations
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-vars)
(require 'init-setup)
(require 'init-builtin)
(require 'init-edit)
(require 'init-ui)
(require 'init-completion)

;; Custom
(unless custom-file
  (load (setq custom-file (locate-user-emacs-file "custom.el")) t))

;; Show startup times
(add-hook 'window-setup-hook
          (lambda ()
            (message "window-setup: %.3fs, after-init: %.3fs"
                     (float-time (time-subtract nil before-init-time))
                     (float-time (time-subtract after-init-time before-init-time)))))
